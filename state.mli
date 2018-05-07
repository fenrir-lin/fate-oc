open Yojson.Basic
open Convert

(********************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will
 * use to test your submission.
 *)

(* [state] is an abstract type representing the state of an adventure. *)
type state

(* [init_state j] is the initial state of the game as
 * determined by JSON object [j].
 * requires: [j] represents an error-free adventure file. *)
val init_state : Yojson.Basic.json -> state

(* [win_score s] is the winning score for the adventure whose current
 * state is represented by [s]. *)
val win_score : state -> int

(* [score s] is the player's current score. *)
val score : state -> int

(* [turns s] is the number of turns the player has taken so far. *)
val turns : state -> int

(* [current_room_id s] is the id of the room in which the adventurer
 * currently is. *)
val current_room_id : state -> string

(* [inv s] is the list of item id's in the adventurer's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant. *)
val inv : state -> string list

(* [visited s] is the list of id's of rooms the adventurer has visited.
 * No room may appear more than once in the list.  Order is irrelevant. *)
val visited : state -> string list

(* [locations s] is an association list mapping item id's to the
 * id of the room in which they are currently located.  Items
 * in the adventurer's inventory are not located in any room.
 * No item may appear more than once in the list.  The relative order
 * of list elements is irrelevant, but the order of pair components
 * is essential:  it must be [(item id, room id)]. *)
val locations : state -> (string*string) list

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].  The function name [do'] is used because
 * [do] is a reserved keyword.  Define the "observable state" to
 * be all the information that is observable about the state
 * from the functions above that take a [state] as input.
 *   - The "go" (and its shortcuts), "take" and "drop" commands
 *     result in an appropriately updated [st'], as described in the
 *     assignment writeup, if their object is valid in
 *     state [st].  If their object is invalid in state [st],
 *     the observable state remains unchanged in [st'].
 *       + The object of "go" is valid if it is a direction by which
 *         the current room may be exited, and if the union of the items
 *         in the player's inventory and the current room contains
 *         all the keys required to move to the target room.
 *       + The object of "take" is valid if it is an item in the
 *         current room.
 *       + The object of "drop" is valid if it is an item in the
 *         current inventory.
 *       + If no object is provided (i.e., the command is simply
 *         the bare word "go", "take", or "drop") the behavior
 *         is unspecified.
 *   - The "quit", "look", "inventory", "inv", "score", and "turns"
 *     commands are always possible and leave the observable state unchanged.
 *   - The behavior of [do'] is unspecified if the command is
 *     not one of the commands given in the assignment writeup.
 * effects: none.  [do'] is not permitted to do any printing as
 *   part of implementing the REPL.  [do'] is not permitted to cause
 *   the engine to terminate.  [do'] is not permitted to raise an exception
 *   unless the precondition is violated.
 * requires: the input state was produced by [init_state] from an
 *   error-free adventure file, or by repeated applications of [do']
 *   to such a state.
 *)
val do' : Command.command -> state -> state

(* END DO NOT CHANGE
 ********************************************************)

(* [do_static command state] returns the string that Main should print out as a
   response to [command] inputted by player. It is "static" because calling
   it does not result in changes in [state].

   Commands that won't change the current state include:
   "look", "inv", "inventory", "score", "turns", "details"
*)
val do_static : Command.command -> state -> string

(*****************************************************************************)
(*** HELPERS OF [do'] AND [do_static] ***)
(*****************************************************************************)

(* [do_look s] returns the appropriate description of current room base on
   player's inventory and items in current room.
*)
val do_look : state -> string

(* [do_inv s] returns a string representing the player's inventory. *)
val do_inv : state -> string

(* [do_score s] returns the current socre. *)
val do_score : state -> string

(* [do_turns s] returns the current turns. *)
val do_turns : state -> string

(* [do_details s] returns a string showing the items in the player's inventory
   and their descrptions.
*)
val do_details : state -> string

(* [do_quit state] returns a changed [state] to singal the player wishes to end
   the game.
*)
val do_quit : state -> state

(* [do_go s] changes current room to the target room, and updates score, turns,
   visited rooms, and message to display accordingly.
*)
val do_go : string -> state -> state

(* [do_take s] adds an item to player's inventory, and updates score, turns,
   and item locations accordingly.
*)
val do_take : string -> state -> state

(* [do_drop s] removes an item from player's inventory, and updates score,
   turns, and item locations accordingly.
*)
val do_drop : string -> state -> state

(* [display_wm s] changes s.wm_displayed to true to represent that the win
   message has been displayed.
*)
val display_wm : state -> state

(*****************************************************************************)
(*** HELPER FUNCTIONS FOR GETTING FIELDS OF [state] FROM OUTSIDE ***)
(*****************************************************************************)

(* [continue s] is true if s.continue is true, false otherwise. It is called
   from Main to decide whether the player wish to continue the game (i.e.
   whether the player entered "quit").
*)
val continue : state -> bool

(* [message s] is the message (description of the room, etc.) that needs to be
   printed out from Main.
*)
val message : state -> string

(* [current_room s] returns the room that the player is in. *)
val current_room : state -> room

(* [locations_map s] returns a LocationMap containing item-room pairs that
   represent the current locations of items.
*)
val locations_map : state -> string LocationMap.t

(* [win_message s] returns the message to be displayed when player wins. *)
val win_message : state -> string

(* [win_message s] is true when win message has been displayed, false
   otherwise.
*)
val wm_displayed : state -> bool

(* [inv_item_list s] is an item list representing the player's current
   inventory.
*)
val inv_item_list : state -> item list

(*****************************************************************************)
(*** HELPER FUNCTIONS FOR INITALIZING ***)
(*****************************************************************************)

(* [start_visited_rooms json] is a set containing only the start room. *)
val start_visited_rooms : json -> RoomSet.t

(* [start_item_locations json] returns a LocationMap containing item-room pairs
   that represent locations of items at the beginning.
*)
val start_item_locations : json -> string LocationMap.t

(* [start_room json] is the start room. *)
val start_room : json -> room

(* [start_description description] picks the first description to display once
   the player starts the game.
*)
val start_description : json -> string

(*****************************************************************************)
(*** OTHER HELPER FUNCTIONS ***)
(*****************************************************************************)

(* [next_room commmand state] returns the room the player will be in once
   command is processed.
*)
val next_room : string -> state -> room

(* [items_in_room_string room location_map] returns a string displaying all the
   items that are currently in [room].
*)
val items_in_room_string : room -> string LocationMap.t -> string

(* [items_in_room room location_map] returns a list of ids of all items in
   current room (not including items in player's inventory).
*)
val items_in_room : room -> string LocationMap.t -> string list

(* [all_items_in_room inv current_room locations] is a list of the ids of all
   the items that are in current_room, including items in the room and items in
   the player's inventory.
*)
val all_items_in_room : item list -> room -> string LocationMap.t -> string list

(* [in_treasure item room] is true if item is in the treasure list of room,
   false otherwise.
*)
val in_treasure : item -> room -> bool

(* [calculate_win_score rooms items] is the sum of the point of each room in
   rooms plus the sum of the point of each item in items.
*)
val calculate_win_score : room list -> item list -> int

(* [calculate_current_score visited_rooms inventory] is the sum of the point of
   each room in visited_rooms plus the sum of the point of each item in
   inventory.
*)
val calculate_current_score :
  room list -> string LocationMap.t -> json -> int

(* [sort_descriptions room] is a list of all descrptions belonging to the room,
   sorted in decreasing order base on the length of description.requires.
*)
val sort_descriptions : description list -> description list

(* [pick_description descrptions] is the appropriate description picked from all
   descriptions of a room base on current inventory.
*)
val pick_description : string list -> description list -> string

(* [to_string state] prints [state] in human-readable form. It's for debugging
   purpose only.
*)
val string_of_state : state -> string
