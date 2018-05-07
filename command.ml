open Convert

(* A type exposed and defined in a .mli file must also be defined in
 * the corresponding .ml file.  So you must repeat the definition
 * of [command] here.  This helps OCaml achieve something called
 * "separate compilation", which you could google for.  Yes,
 * it's a little bit annoying, but there is a good reason for it. *)

(* [valid] field denotes whether this is a valid command. If valid = false,
   [err_message] will be returned to Main and get printed out.
   [err_message] contains information about why this command is invalid,
   examples include "there's no room in that direction" or "this item is
   not in your inventory."
   [action] is the verb in a verb + noun command. Valid actions are defined
   in commands1 - commands3.
   [target] is the target of actions that take an argument (go, take, and drop)
*)
type command = {
  valid : bool;
  err_message : string;
  action : string;
  target : string
}

(* commands1 are names of commands that does not take any argument, and does not
   change the state.
*)
let commands1 = ["look"; "inv"; "inventory"; "score"; "turns"; "details"]

(* commands2 are names of commands that does not take any argument, but does
   result in change in current state.
*)
let commands2 = ["quit"]

(* commands2 are names of commands that takes an argument (direction, item,
   etc.) and does change the state.
*)
let commands3 = ["go"; "take"; "drop"]

(* [parse str] turns the string put in by player to type [command]. *)
let parse str =
  match String.split_on_char ' ' (String.lowercase_ascii str) with
  | [] ->
    {valid = true; err_message = "";
     action = ""; target = ""}
  | x :: [] ->
    if List.mem x (commands1 @ commands2) then
      {valid = true; err_message = "";
       action = x; target = ""}
    else if x = "take" then
      {valid = false; err_message = "Please type TAKE + item name.";
       action = x; target = ""}
    else if x = "drop" then
      {valid = false; err_message = "Please type DROP + item name.";
       action = x; target = ""}
    else if x = "go" then
      {valid = false; err_message = "Where would you like to go?";
       action = x; target = ""}
    else
      {valid = true; err_message = "";
       action = "go"; target = x}
  | x :: xs ->
    {valid = true; err_message = "";
     action = x; target = String.concat " " xs}
