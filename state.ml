(* [state] represents the state of an adventure. *)
open Yojson.Basic.Util
open Convert
open Command

(*****************************************************************************
 * TYPES
 *****************************************************************************)

(* [state] represents a game state. Its has the following fields:
    - [continue] is true if the player hasn't typed "quit", false otherwise.
    - [win_score] is the sum of scores of all rooms and items.
    - [current_score] is player's current score.
    - [current_turn] is player's current turn.
    - [inventory] is player's inventory.
    - [current_room] is player's current room.
    - [visited_rooms] is all rooms the player has visited.
    - [item_locations] is the location of all items not in inventory.
    - [message] is the description of current room plus a list of all items
      in the room.
    - [full_room_set] is the set of all rooms in the game file.
    - [full_item_set] is the set of all items in the game file.
    - [wm_displayed] is true if the win message has been displayed, false
      otherwise.
*)
type state = {
  continue       : bool;
  win_score      : int;
  current_score  : int;
  current_turn   : int;
  inventory      : item list;
  current_room   : room;
  visited_rooms  : RoomSet.t;
  item_locations : string LocationMap.t;
  message        : string;
  full_room_set  : RoomSet.t;
  full_item_set  : ItemSet.t;
  win_message    : string;
  wm_displayed   : bool;
}

(*****************************************************************************
 * FUNCTIONS
 *****************************************************************************)

let calculate_win_score rooms items =
  (List.fold_left (+) 0 (item_lst_to_pt_lst items)) +
  (List.fold_left (+) 0 (room_lst_to_pt_lst rooms))

let calculate_current_score visited_rooms locations j =
  (List.fold_left (+) 0 (room_lst_to_pt_lst visited_rooms)) +
  (List.fold_left (+) 0
     (item_lst_to_pt_lst (get_items_in_treasure_rm locations j)))

let items_in_room (room : room) locations =
 LocationMap.key_list_string
   (LocationMap.filter (fun k v -> v = room.id) locations)

let items_in_room_string (room : room) locations =
  String.concat ""
   (List.map (fun e -> "\n * There is a " ^ e) (items_in_room room locations))

let all_items_in_room inv room locations =
 item_lst_to_id_lst inv @ items_in_room room locations

let start_visited_rooms j = RoomSet.add (parse_start_room j) RoomSet.empty

let start_item_locations = parse_start_locations

let start_room = parse_start_room

let sort_descriptions descrptions =
  List.rev
    (List.sort
       (fun d1 d2 -> List.length d1.requires - List.length d2.requires)
       descrptions)

let compare_length d1 d2 = List.length d2.requires - List.length d1.requires

let rec pick_description inv descriptions =
  match List.sort compare_length descriptions with
  | [] -> "This room has no description."
  | x :: [] -> x.text
  | x :: xs ->
    if is_sublist (List.sort String.compare x.requires)
        (List.sort String.compare inv)
    then x.text else pick_description inv xs

let start_description j =
  pick_description (all_items_in_room (parse_start_inv j)
                      (start_room j) (start_item_locations j))
    (start_room j).description

let init_state j = {
  continue = true;
  win_score = calculate_win_score (all_rooms j) (all_items j);
  current_score = calculate_current_score
      (RoomSet.elements (start_visited_rooms j)) (start_item_locations j) j;
  current_turn = 0;
  inventory = parse_start_inv j;
  current_room = start_room j;
  visited_rooms = start_visited_rooms j;
  item_locations = start_item_locations j;
  message = (start_description j) ^
            (items_in_room_string (start_room j) (start_item_locations j));
  full_room_set = parse_rooms j;
  full_item_set = parse_items j;
  win_message = parse_win_message j;
  wm_displayed = false
}

let continue s = s.continue

let win_score s = s.win_score

let score s = s.current_score

let turns s = s.current_turn

let current_room_id s = s.current_room.id

let inv s = item_lst_to_id_lst s.inventory

let visited s = rms_to_id_lst s.visited_rooms

let locations s = LocationMap.bindings s.item_locations

let message s = s.message

let current_room s = s.current_room

let locations_map s = s.item_locations

let win_message s = s.win_message

let wm_displayed s = s.wm_displayed

let inv_item_list s = s.inventory

let display_wm s = {
  s with
  wm_displayed = true
}

let do_look s =
  pick_description
    (all_items_in_room s.inventory s.current_room s.item_locations)
    s.current_room.description
  ^ items_in_room_string s.current_room s.item_locations

let do_inv s =
  "You have the following items in your inventory:" ^
  String.concat ""
    (List.map (fun s -> "\n - " ^ s)
       (List.sort String.compare (item_lst_to_id_lst s.inventory))) ^
  "\n\n(TIPS: type DETAILS to view item descriptions.)"

let do_score s = "Your current score is: " ^ string_of_int s.current_score

let do_turns s = "Your current turns is: " ^ string_of_int s.current_turn

let do_details s =
  "Here're items in your inventory and their descriptions:" ^
  String.concat ""
    (List.map (fun s -> "\n - " ^ s)
       (List.sort String.compare (item_id_plus_description s.inventory)))

let do_static c s =
  if c.valid then
    if      c.action = "look"                          then do_look s
    else if c.action = "inv" || c.action = "inventory" then do_inv s
    else if c.action = "score"                         then do_score s
    else if c.action = "turns"                         then do_turns s
    else if c.action = "details"                       then do_details s
    else "Invalid command!"
  else c.err_message

let do_quit s = {
  s with
  continue = false;
}

let find_exit target room =
  List.find (fun e -> target = e.direction) room.exits

let next_room target s =
  List.find
    (fun (r : room) -> r.id = (find_exit target s.current_room).room_id)
    (RoomSet.elements s.full_room_set)

let do_go target s =
  let next_room = next_room target s
  in {
    s with
    current_score = (+) s.current_score
        (if not (RoomSet.mem next_room s.visited_rooms)
         then next_room.point
         else 0);
    current_turn = s.current_turn + 1;
    current_room = next_room;
    visited_rooms = RoomSet.add next_room s.visited_rooms;
    message =
      pick_description
        (all_items_in_room s.inventory next_room s.item_locations)
        next_room.description ^
      items_in_room_string next_room s.item_locations;
  }

let in_treasure item room =
  List.mem item.id room.treasure

let do_take target s =
  let item =
    List.find (fun e -> target = String.lowercase_ascii e.id)
      (ItemSet.elements s.full_item_set)
  in {
    s with
    current_score = (-) s.current_score
        (if in_treasure item s.current_room then item.point else 0);
    current_turn = s.current_turn + 1;
    inventory = item :: s.inventory;
    item_locations = LocationMap.remove item.id s.item_locations;
  }

let do_drop target s =
  let item = List.find
      (fun e -> target = String.lowercase_ascii e.id) s.inventory
  in {
    s with
    current_score = (+) s.current_score
        (if in_treasure item s.current_room then item.point else 0);
    current_turn = s.current_turn + 1;
    inventory = List.filter (fun e -> e.id <> item.id) s.inventory;
    item_locations = LocationMap.add item.id s.current_room.id s.item_locations;
  }

let do' c st =
  if      c.action = "quit" then do_quit st
  else if c.action = "go"   then do_go   c.target st
  else if c.action = "take" then do_take c.target st
  else if c.action = "drop" then do_drop c.target st
  else st

let string_of_state s =
    "\n{" ^
    "\ncontinue = " ^ string_of_bool s.continue ^
    "\nwin_score = " ^ string_of_int s.win_score ^
    "\ncurrent_score = " ^ string_of_int s.current_score ^
    "\ncurrent_turn = " ^ string_of_int s.current_turn ^
    "\ninventory = " ^ "[" ^ String.concat ", "
      (item_lst_to_id_lst s.inventory) ^ "]" ^
    "\ncurrent_room = " ^ s.current_room.id ^
    "\nvisited_rooms = " ^ "[" ^ String.concat ", "
      (room_lst_to_id_lst (RoomSet.elements s.visited_rooms)) ^ "]" ^
    "\nitem_locations = " ^ "[" ^ String.concat ", "
      (bindings_to_string_lst (LocationMap.bindings s.item_locations)) ^ "]" ^
    "\nmessage = " ^ s.message ^
    "\nfull_room_set = " ^ "[" ^ String.concat ", "
      (room_lst_to_id_lst (RoomSet.elements s.full_room_set)) ^ "]" ^
    "\nfull_item_set = " ^ "[" ^ String.concat ", "
      (item_lst_to_id_lst (ItemSet.elements s.full_item_set)) ^ "]" ^
    "\nwin_message = " ^ s.win_message ^
    "\nwm_displayed = " ^ string_of_bool s.wm_displayed ^
    "\n}"
