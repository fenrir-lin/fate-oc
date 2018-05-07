open OUnit2
open State

(*******************************************************************)
(* Helper values used throughout this test suite. *)
(*******************************************************************)

(*** input files ***)
let j1 = Yojson.Basic.from_file "oneroom.json"
let j2 = Yojson.Basic.from_file "threerooms.json"

(*** commands ***)
let c_look : Command.command =
  {valid = true; err_message = ""; action = "look"; target = ""}
let c_inv : Command.command =
  {valid = true; err_message = ""; action = "inv"; target = ""}
let c_inventory : Command.command =
  {valid = true; err_message = ""; action = "inventory"; target = ""}
let c_score : Command.command =
  {valid = true; err_message = ""; action = "score"; target = ""}
let c_turns : Command.command =
  {valid = true; err_message = ""; action = "turns"; target = ""}
let c_take_1 : Command.command =
  {valid = true; err_message = ""; action = "take"; target = "item1"}
let c_drop_1 : Command.command =
  {valid = true; err_message = ""; action = "drop"; target = "item1"}
let c_take_2 : Command.command =
  {valid = true; err_message = ""; action = "take"; target = "red hat"}
let c_drop_2 : Command.command =
  {valid = true; err_message = ""; action = "drop"; target = "red hat"}
let c_go : Command.command =
  {valid = true; err_message = ""; action = "go"; target = "north"}
let c_quit : Command.command =
  {valid = true; err_message = ""; action = "quit"; target = ""}

(*** states for oneroom.json ***)
let init_state_1      = init_state j1
let after_look_1      = do' c_look init_state_1
let after_inv_1       = do' c_inv after_look_1
let after_inventory_1 = do' c_inventory after_inv_1
let after_score_1     = do' c_score after_inventory_1
let after_turns_1     = do' c_turns after_score_1
let after_take_1      = do' c_take_1 after_turns_1
let after_drop_1      = do' c_drop_1 after_take_1
let after_quit_1      = do' c_quit after_drop_1

(*** states for threerooms.json ***)
let init_state_2      = init_state j2
let after_look_2      = do' c_look init_state_2
let after_inv_2       = do' c_inv after_look_2
let after_inventory_2 = do' c_inventory after_inv_2
let after_score_2     = do' c_score after_inventory_2
let after_turns_2     = do' c_turns after_score_2
let after_take_2      = do' c_take_2 after_turns_2
let after_drop_2      = do' c_drop_2 after_take_2
let after_go_2        = do' c_go after_drop_2
let after_quit_2      = do' c_quit after_drop_2

(*******************************************************************)
(* Test cases. *)
(*******************************************************************)

(* Test the initial states. *)
let tests = [
  "win_score1" >:: (fun _ -> assert_equal 110   (init_state_1 |> win_score));
  "win_score2" >:: (fun _ -> assert_equal 11111 (init_state_2 |> win_score));
  "score1" >:: (fun _ -> assert_equal 110   (init_state_1 |> score));
  "score2" >:: (fun _ -> assert_equal 10001 (init_state_2 |> score));
  "turns1" >:: (fun _ -> assert_equal 0 (init_state_1 |> turns));
  "turns2" >:: (fun _ -> assert_equal 0 (init_state_2 |> turns));
  "current_room1" >:: (fun _ -> assert_equal
                          "room1" (init_state_1 |> current_room_id));
  "current_room2" >:: (fun _ -> assert_equal
                          "room1" (init_state_2 |> current_room_id));
  "inv1" >:: (fun _ -> assert_equal [] (init_state_1 |> inv));
  "inv2" >:: (fun _ -> assert_equal ["white hat"] (init_state_2 |> inv));
  "visited1" >:: (fun _ -> assert_equal ["room1"] (init_state_1 |> visited));
  "visited2" >:: (fun _ -> assert_equal ["room1"] (init_state_2 |> visited));
  "locations1" >:: (fun _ -> assert_equal
                       [("item1","room1")] (init_state_1 |> locations));
  "locations2" >:: (fun _ -> assert_equal
                       (List.sort Pervasives.compare
                          [("key","room2");("black hat","room1");("red hat","room1")])
                       (List.sort Pervasives.compare (init_state_2 |> locations)));
]

(* Test states after a series of actions. *)
let do_tests = [
  (* assert that after a series of commands that are not supposed to change
     state, the state is in fact not changed. *)
  "win_score1" >:: (fun _ -> assert_equal 110   (after_turns_1 |> win_score));
  "win_score2" >:: (fun _ -> assert_equal 11111 (after_turns_2 |> win_score));
  "score1"     >:: (fun _ -> assert_equal 110   (after_turns_1 |> score));
  "score2"     >:: (fun _ -> assert_equal 10001 (after_turns_2 |> score));
  "turns1"     >:: (fun _ -> assert_equal 0 (after_turns_1 |> turns));
  "turns2"     >:: (fun _ -> assert_equal 0 (after_turns_2 |> turns));
  "current_room1" >:: (fun _ -> assert_equal
                          "room1" (after_turns_1 |> current_room_id));
  "current_room2" >:: (fun _ -> assert_equal
                          "room1" (after_turns_2 |> current_room_id));
  "inv1"       >:: (fun _ -> assert_equal [] (after_turns_1 |> inv));
  "inv2"       >:: (fun _ -> assert_equal ["white hat"] (after_turns_2 |> inv));
  "visited1"   >:: (fun _ -> assert_equal ["room1"] (after_turns_1 |> visited));
  "visited2"   >:: (fun _ -> assert_equal ["room1"] (after_turns_2 |> visited));
  "locations1" >:: (fun _ -> assert_equal
                       [("item1","room1")] (after_turns_1 |> locations));
  "locations2" >:: (fun _ -> assert_equal
                       (List.sort Pervasives.compare
                          [("key","room2");("black hat","room1");("red hat","room1")])
                       (List.sort Pervasives.compare (after_turns_2 |> locations)));
  (* assert that after the TAKE command, states are changed correctly. *)
  (* oneroom *)
  "take_win_score1" >:: (fun _ -> assert_equal 110 (after_take_1 |> win_score));
  "take_score1"     >:: (fun _ -> assert_equal 10 (after_take_1 |> score));
  "take_turns1"     >:: (fun _ -> assert_equal 1 (after_take_1 |> turns));
  "take_current_room1" >:: (fun _ -> assert_equal
                               "room1" (after_take_1 |> current_room_id));
  "take_inv1" >:: (fun _ -> assert_equal ["item1"] (after_take_1 |> inv));
  "take_visited1" >:: (fun _ -> assert_equal ["room1"] (after_take_1 |> visited));
  "take_locations1" >:: (fun _ -> assert_equal [] (after_take_1 |> locations));
  (* threerooms *)
  "take_win_score2" >:: (fun _ -> assert_equal 11111 (after_take_2 |> win_score));
  "take_score2"     >:: (fun _ -> assert_equal 1 (after_take_2 |> score));
  "take_turns2"     >:: (fun _ -> assert_equal 1 (after_take_2 |> turns));
  "take_current_room2" >:: (fun _ -> assert_equal
                               "room1" (after_take_2 |> current_room_id));
  "take_inv2" >:: (fun _ -> assert_equal
                      (List.sort String.compare ["white hat"; "red hat"])
                      (List.sort String.compare (after_take_2 |> inv)));
  "take_visited2" >:: (fun _ -> assert_equal ["room1"] (after_take_2 |> visited));
  "take_locations2" >:: (fun _ -> assert_equal
                            (List.sort Pervasives.compare
                               [("key","room2");("black hat","room1")])
                            (List.sort Pervasives.compare (after_take_2 |> locations)));
  (* assert after the DROP command, states are changed correctly. *)
  (* oneroom *)
  "drop_win_score1" >:: (fun _ -> assert_equal 110 (after_drop_1 |> win_score));
  "drop_score1"     >:: (fun _ -> assert_equal 110 (after_drop_1 |> score));
  "drop_turns1"     >:: (fun _ -> assert_equal 2 (after_drop_1 |> turns));
  "drop_current_room1" >:: (fun _ -> assert_equal
                               "room1" (after_drop_1 |> current_room_id));
  "drop_inv1"     >:: (fun _ -> assert_equal [] (after_drop_1 |> inv));
  "drop_visited1" >:: (fun _ -> assert_equal ["room1"] (after_drop_1 |> visited));
  "drop_locations1" >:: (fun _ -> assert_equal
                            [("item1","room1")] (after_drop_1 |> locations));
  (* threerooms *)
  "drop_win_score2" >:: (fun _ -> assert_equal 11111 (after_drop_2 |> win_score));
  "drop_score2"     >:: (fun _ -> assert_equal 10001 (after_drop_2 |> score));
  "drop_turns2"     >:: (fun _ -> assert_equal 2 (after_drop_2 |> turns));
  "drop_current_room2" >:: (fun _ -> assert_equal
                               "room1" (after_drop_2 |> current_room_id));
  "drop_inv2" >:: (fun _ -> assert_equal ["white hat"] (after_drop_2 |> inv));
  "drop_visited2" >:: (fun _ -> assert_equal ["room1"] (after_drop_2 |> visited));
  "drop_locations2" >:: (fun _ -> assert_equal
                            (List.sort Pervasives.compare
                               [("key","room2");("black hat","room1");("red hat","room1")])
                            (List.sort Pervasives.compare (after_drop_2 |> locations)));
  (* assert after the GO command, states are changed correctly. *)
  "go_win_score2" >:: (fun _ -> assert_equal 11111 (after_go_2 |> win_score));
  "go_score2"     >:: (fun _ -> assert_equal 10011 (after_go_2 |> score));
  "go_turns2"     >:: (fun _ -> assert_equal 3 (after_go_2 |> turns));
  "go_current_room2" >:: (fun _ -> assert_equal
                               "room2" (after_go_2 |> current_room_id));
  "go_inv2" >:: (fun _ -> assert_equal ["white hat"] (after_go_2 |> inv));
  "go_visited2" >:: (fun _ -> assert_equal
                        (List.sort String.compare ["room1";"room2"])
                        (List.sort String.compare (after_go_2 |> visited)));
  "go_locations2" >:: (fun _ -> assert_equal
                            (List.sort Pervasives.compare
                               [("key","room2");("black hat","room1");("red hat","room1")])
                            (List.sort Pervasives.compare (after_go_2 |> locations)));
  (* assert after the QUIT command, states are changed correctly. *)
  "quit_1" >:: (fun _ -> assert_equal false (continue after_quit_1));
  "quit_2" >:: (fun _ -> assert_equal false (continue after_quit_2));
]

let suite =
  "Adventure test suite" >::: List.flatten [
    tests;
    do_tests;
  ]

let _ = run_test_tt_main suite
