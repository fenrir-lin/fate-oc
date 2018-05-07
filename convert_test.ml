open OUnit2
open Convert

let j = read_file "threerooms.json"

let tests =
  [
    "start_room" >:: (fun _ -> assert_equal "room1" parse_start_room (read_start_room j));
  ]

let suite =
  "Adventure test suite"
  >::: tests

let _ = run_test_tt_main suite
