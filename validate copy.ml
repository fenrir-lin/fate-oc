open Convert
open Command
open State

(*****************************************************************************
 * DIFFERENT INVALID COMMANDS TO BE RETURNED
 *****************************************************************************)

let undefined_command = {
  valid = false; err_message = "The command you typed in is undefined!";
  action = ""; target = ""
}

let too_many_arguments = {
  valid = false; err_message = "This command takes no argument!";
  action = ""; target = ""
}

let missing_key = {
  valid = false;
  err_message =
    "You can't enter that room because you are missing some required item(s)";
  action = "";
  target = ""
}

let no_such_exit = {
  valid = false; err_message = "There is nothing in that direction!";
  action = ""; target = ""
}

let already_in_inv = {
  valid = false; err_message = "This item is already in your inventory!";
  action = ""; target = ""
}

let no_such_item = {
  valid = false; err_message = "There is no such item in this room!";
  action = ""; target = ""
}

let not_in_inv = {
  valid = false; err_message = "This item is not in your inventory!";
  action = ""; target = ""
}

let unknown_error = {
  valid = false;
  err_message = "Some unknown error occurs. Please doublecheck the game file.";
  action = ""; target = ""
}

(*****************************************************************************
 * FUNCTIONS
 *****************************************************************************)

(* [get_exits_list room] is the list of all exits of [room]. *)
let get_exits_list room =
  List.map (fun e -> e.direction) room.exits

(* [item_is_in_room item state] returns true if [item] is in current room,
   false otherwise.
*)
let item_is_in_room it st =
  Pervasives.snd
    (List.find (fun (k,v) -> it = String.lowercase_ascii k)
       (LocationMap.bindings (locations_map st))) = current_room_id st

(* [validate_commands12 command] is [command] if target field is empty,
   otherwise return an invalid command with appropriate error message.

   requires: [command.action] is one of the command in [Command.commands1] or
   [Command.commands2].
*)
let validate_commands12 c =
  if c.target = "" then c
  else too_many_arguments

(* [validate_go command state] changes [state] according to [command] if it
   is valid (the exit exists and all required keys are present), or return
   an invalid command with error message otherwise.

   requires: [command.action = "go"]
*)
let validate_go c s =
  let exits_id_list = get_exits_list (current_room s) in
  let exits_list = (current_room s).exits in
  if List.mem c.target exits_id_list then
    if is_sublist
        (List.find
           (fun e -> e.direction = c.target) exits_list).keys
        ((inv s) @ items_in_room (current_room s) (locations_map s))
    then c
    else missing_key
  else no_such_exit

(* [validate_take command state] changes [state] according to [command] if it
   is valid (the item is in current room), or return an invalid command with
   error message otherwise.

   requires: [command.action = "take"]
*)
let validate_take c s =
  if List.mem c.target (inv s) then
    already_in_inv
  else
    try
      if item_is_in_room c.target s then c
      else no_such_item
    with
    | Not_found -> no_such_item
    | _ -> unknown_error

(* [validate_drop command state] changes [state] according to [command] if it
   is valid (the item is in inventory), or return an invalid command with
   error message otherwise.

   requires: [command.action = "drop"]
*)
let validate_drop c s =
  if not (List.mem c.target
            (List.map(fun s -> String.lowercase_ascii s) (inv s)))
  then not_in_inv
  else c

(* When [c.valid = true], [validate c s] tells whether command [c] is truly
   valid given current state [s]. If [c] is valid, the command is returned to
   Main unchanged, otherwise a command with error message will be returned.
   When [c.valid = false], [validate c s] returns [c] unchanged.
*)
let validate c s =
  if not c.valid then c
  else if List.mem c.action (commands1 @ commands2) then validate_commands12 c
  else if c.action = "go"   then validate_go c s
  else if c.action = "take" then validate_take c s
  else if c.action = "drop" then validate_drop c s
  else undefined_command
