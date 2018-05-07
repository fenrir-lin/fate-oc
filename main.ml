(*
 * CS 3110 Fall 2017 A2
 * Author: Zifan Lin
 * NetID: zl433
 *
 * Acknowledge here any contributions made to your solution that
 * did not originate from you or from the course staff:
 *
 *)
open Convert
open Command
open State
open Validate

(* [get_new_state st] takes in a game state and returns a new state based on
   the player's command, while printing out necessary messages.

   requires: [st] is a valid game state.
*)
let get_new_state st f =
  (* print_endline (string_of_state st); (* FOR DEBUGGING ONLY *) *)
  if (win_score st <= score st) && (not (wm_displayed st)) then
    (print_endline (win_message st); display_wm st)
  else begin
    print_string "\n>>> ";
    let c = validate (parse (Pervasives.read_line ())) st in
    if c.valid then
      if List.mem c.action commands1 then
        (print_endline (do_static c st); st)
      else if c.action = "go" then
        (print_endline (message (do' c st)); do' c st)
      else if c.action = "take" then
        (print_endline "The item is now in your inventory."; do' c st)
      else if c.action = "drop" then
        (print_endline "The item is removed from your inventory."; do' c st)
      else
        do' c st
    else
      (print_endline c.err_message; st)
  end

(* [game_loop state] is the main loop that takes in command from player and
   change game state accordingly. It goes on forever until [state.continue]
   is false.
*)
let rec game_loop st f =
  if continue st then game_loop (get_new_state st f) f
  else print_endline "Exited from game.\n"

(* [print_start_message init_state] prints the description of the first room
   plus all items in the starting room.
*)
let print_start_message init_state =
  print_endline (message init_state)

(* [play_game f] plays the game in adventure file [f]. *)
let play_game f =
  try
    print_endline (sanity_check f);
    let init_state = init_state (read_file f) in
    print_start_message init_state;
    game_loop init_state f
  with
  | _ -> print_endline
           "Some error occurs. Please doublecheck the name of the game file
or the game file itself."

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

let () = main ()
