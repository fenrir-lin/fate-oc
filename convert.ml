(* [convert] reads in JSON file and converts it into OCaml types.
   This module also contain functions that do conversions between sets, lists,
   dictionaries, etc. Some sanity check functions are also contained.
*)
open Yojson.Basic
open Yojson.Basic.Util

(*****************************************************************************
 * TYPES
 *****************************************************************************)

(* [description] represents one description of a room. *)
type description = {
  requires: string list;
  text: string
}

(* [exit] represents one exit of a room. *)
type exit = {
  direction: string;
  room_id: string;
  keys: string list
}

(* [room] represents a room. *)
type room = {
  id: string;
  description: description list;
  point: int;
  exits: exit list;
  treasure: string list
}

(* [item] represents an item. *)
type item = {
  id: string;
  description: string;
  point: int
}

(*****************************************************************************
 * MODULES
 *****************************************************************************)

module Room = struct
  type t = room

  let compare (r1 : room) (r2 : room) = String.compare r1.id r2.id
end

module Item = struct
  type t = item

  let compare (i1 : item) (i2 : item) = String.compare i1.id i2.id
end

module RoomSet = Set.Make(Room)

module ItemSet = Set.Make(Item)

module StringMap = Map.Make(String)

(* [LocationMap] maps item id to room id that the item is in. *)
module LocationMap = struct
  include StringMap

  (* [key_list_string] returns a list of ids of all items in the map. *)
  let key_list_string lm =
    List.map (fun (k, v) -> k) (bindings lm)

  (* [key_list_string] returns a list of all items in the map. *)
  let key_list_item lm id_to_item j =
    List.map (fun (k, v) -> id_to_item j k) (bindings lm)

  (* [key_list_string] returns a list of ids of all rooms in the map. *)
  let value_list_string lm =
    List.map (fun (k, v) -> v) (bindings lm)
end

(*****************************************************************************
 * FUNCTIONS
 *****************************************************************************)

(************************************************************************)
(* READ FUNCTIONS : these functions read the whole or part of a JSON file. *)

(* [read_file f] returns a JSON read from game file [f]. *)
let read_file f = from_file f

(* [read_rooms json] returns the "rooms" field of the JSON. *)
let read_rooms j = to_list (member "rooms" j)

(* [read_items json] returns the "items" field of the JSON. *)
let read_items j = to_list (member "items" j)

(* [read_start_room json] returns the "start_room" field of the JSON. *)
let read_start_room j = member "start_room" j

(* [read_start_inv json] returns the "start_inv" field of the JSON. *)
let read_start_inv j = member "start_inv" j

(* [read_start_locations json] returns the "start_locations" field of the
   JSON.
*)
let read_start_locations j = to_list (member "start_locations" j)

(* [read_win_message json] returns the "win_message" field of the JSON. *)
let read_win_message j = member "win_message" j

(************************************************************************)
(* PARSE FUNCTIONS : these functions turn JSON into OCaml types. *)

(* [start_inv_str_list json] returns a list of item ids in start_inv. *)
let start_inv_str_list j = List.map to_string (to_list (read_start_inv j))

(* [parse_description j] parse a JSON representing one description into
   OCaml type.

   requires: [j] represents a description.
*)
let parse_description j = {
  requires = List.map to_string (to_list (member "requires" j));
  text = to_string (member "text" j)
}

(* [parse_description j] parse a JSON representing one exit into OCaml type.

   requires: [j] represents an exit.
*)
let parse_exit j = {
  direction = to_string (member "direction" j);
  room_id = to_string (member "room_id" j);
  keys = List.map to_string (to_list (member "keys" j))
}

(* [parse_description j] parse a JSON representing one room into OCaml type.

   requires: [j] represents a room.
*)
let parse_room j ={
  id = to_string (member "id" j);
  description = List.map parse_description (to_list (member "descriptions" j));
  point = to_int (member "points" j);
  exits = List.map parse_exit (to_list (member "exits" j));
  treasure = List.map to_string (to_list (member "treasure" j))
}

(* [parse_description j] parse a JSON representing one item into OCaml type.

   requires: [j] represents an item.
*)
let parse_item j = {
  id = to_string (member "id" j);
  description = to_string (member "description" j);
  point = to_int (member "points" j)
}

(* [parse_description j] parse a JSON representing one item-room pair in
   "start_locations" into a tuple, where the first element is a JSON
   representing an item, and the second element is a JSON representing
   a room.

   requires: [j] is an item-room pair in "start_locations".
*)
let parse_start_location j =
  (to_string (member "item" j), to_string (member "room" j))

(* [parse_rooms j] turns a JSON representing a list of rooms into OCaml set.

   requires: [j] represents a list of rooms.
*)
let parse_rooms j = RoomSet.of_list (List.map parse_room (read_rooms j))

(* [parse_items j] turns a JSON representing a list of items into OCaml set.

   requires: [j] represents a list of items.
*)
let parse_items j = ItemSet.of_list (List.map parse_item (read_items j))

(* [all_rooms j] returns a list of all rooms present in [j]. *)
let all_rooms j = RoomSet.elements (parse_rooms j)

(* [all_items j] returns a list of all items present in [j]. *)
let all_items j = ItemSet.elements (parse_items j)

(* [parse_start_room j] returns the start room. *)
let parse_start_room j =
  List.find
    (fun (r :room) -> r.id = to_string (read_start_room j)) (all_rooms j)

(* [parse_start_inv j] returns the start inventory. *)
let parse_start_inv j =
  List.filter
    (fun (i : item) -> List.mem i.id (start_inv_str_list j)) (all_items j)

(* [parse_start_locations j] turns item-room pairs in "start_locations" field
   of JSON [j] into OCaml map.
*)
let parse_start_locations j =
  let kv_pairs = List.map parse_start_location (read_start_locations j) in
  List.fold_right (fun (k, v) -> LocationMap.add k v) kv_pairs LocationMap.empty

(* [parse_win_message j] returns the win message. *)
let parse_win_message j = to_string (read_win_message j)

(************************************************************************)
(* CONVERTION FUNCTIONS : these functions convert between OCaml type, for
   example, item list to item id (string) list, RoomSet to room list, etc.
*)

(* [id_to_room json roomid] returns a room with the same id as [roomid]. *)
let id_to_room j rid = List.find (fun (r : room) -> r.id = rid) (all_rooms j)

(* [id_to_item json itemid] returns an item with the same id as [itemid]. *)
let id_to_item j iid = List.find (fun (i : item) -> i.id = iid) (all_items j)

(* [item_lst_to_id_lst item_list] turns a list of items to a list of
   ids of items.
*)
let item_lst_to_id_lst ilst = List.map (fun (i : item) -> i.id) ilst

(* [room_lst_to_id_lst room_list] turns a list of rooms to a list of
   ids of rooms.
*)
let room_lst_to_id_lst rlst = List.map (fun (r : room) -> r.id) rlst

(* [item_lst_to_pt_lst item_list] turns a list of items to a list of
   points of items.
*)
let item_lst_to_pt_lst ilst = List.map (fun (i : item) -> i.point) ilst

(* [room_lst_to_pt_lst room_list] turns a list of rooms to a list of
   points of rooms.
*)
let room_lst_to_pt_lst rlst = List.map (fun (r : room) -> r.point) rlst

(* [rms_to_id_lst room_list] turns a set of rooms to a list of ids of rooms. *)
let rms_to_id_lst rms = List.map (fun (r : room) -> r.id) (RoomSet.elements rms)

(* [ims_to_id_lst room_list] turns a set of items to a list of ids of items. *)
let ims_to_id_lst ims = List.map (fun (i : item) -> i.id) (ItemSet.elements ims)

(* [all_room_id json] returns a list of all room ids in the JSON. *)
let all_room_id j = room_lst_to_id_lst (all_rooms j)

(* [all_item_id json] returns a list of all item ids in the JSON. *)
let all_item_id j = item_lst_to_id_lst (all_items j)

(* [get_elements_in_treasure_rm location_map json] return a list of items
   that are located in their treasure room.
*)
let get_items_in_treasure_rm lom j =
  LocationMap.key_list_item (LocationMap.filter
    (fun k v -> List.mem k (id_to_room j v).treasure) lom) id_to_item j

(* [bindings_to_string_lst bindings] turns the bindings of a LocationMap
   into a list of strings.
*)
let bindings_to_string_lst bindings =
  List.map (fun (k, v) -> "(" ^ k ^ ", " ^ v ^ ")") bindings

(* [item_id_plus_description item_list] turns a list of items into a
   list of strings, and each string is item id plus item description.
*)
let item_id_plus_description ilst =
  List.map (fun i -> i.id ^ " : " ^ i.description) ilst

(************************************************************************)
(* SANITY CHECK FUNCTIONS : these functions check if a structural correct
   game file contains errors.
*)

(* [is_sublist l1 l2] is true if all members of l1 are also members of l2,
   false otherwise.
 *)
 let rec is_sublist l1 l2 =
  match l1 with
  | [] -> true
  | x :: xs -> List.mem x l2 && is_sublist xs l2

(* [sanity_check_1 j] checks if there is any exit pointing to nonexistent
   room. Returns an empty string if [j] passes the test, returns an error
   message otherwise.
*)
let sanity_check_1 j =
  let rooms = all_room_id j in
  let exits_list = List.sort_uniq String.compare
      (List.map (fun e -> e.room_id)
         (List.concat (List.map (fun r -> r.exits) (all_rooms j)))) in
  if is_sublist exits_list rooms then ""
  else
    "\nSanity check failed: the following exit.room_id is not in
room list:\n" ^ "[" ^
    (String.concat ", "
       (List.filter (fun e -> not (List.mem e rooms)) exits_list)) ^ "]"

(* [sanity_check_2 j] checks if there is any item in start locations not in all
   items list. Returns an empty string if [j] passes the test, returns an error
   message otherwise.
*)
let sanity_check_2 j =
  let items = item_lst_to_id_lst (all_items j) in
  let locations = parse_start_locations j in
  let keys =
    List.sort_uniq String.compare (LocationMap.key_list_string locations) in
  if is_sublist keys items then ""
  else
    "\nSanity check failed: the following items are in start locations
but not in item list:\n" ^ "[" ^
      (String.concat ", "
         (List.filter (fun e -> not (List.mem e items)) keys)) ^ "]"

(* [sanity_check_3 j] checks if there is any room in start locations not in all
   rooms list. Returns an empty string if [j] passes the test, returns an error
   message otherwise.
*)
let sanity_check_3 j =
  let rooms = all_room_id j in
  let locations = parse_start_locations j in
  let values =
    List.sort_uniq String.compare (LocationMap.value_list_string locations) in
  if is_sublist values rooms then ""
  else
    "\nSanity check failed: the following items are in start location but
not in item list:\n" ^ "[" ^
    (String.concat ", "
       (List.filter (fun e -> not (List.mem e rooms)) values)) ^ "]"

(* Run a series of sanity check functions to see if a structural correct JSON
   contains errors. Returns an empty string if the JSON passes all tests,
   return an error message otherwise.
*)
let sanity_check f =
  let j = read_file f in
  sanity_check_1 j ^ sanity_check_2 j ^ sanity_check_3 j
