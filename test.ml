open OUnit2
open Levels
open Player_state

let string_of_tuple (tup : int * int) =
  let nums =
    String.concat ","
      [ string_of_int (fst tup); string_of_int (snd tup) ]
  in
  String.concat "" [ "("; nums; ")" ]

(** TODO: add tile_type to printing*)
let print_tile (tile : tile) =
  String.concat " " [ string_of_tuple tile.pos ]

let entrance_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (entrance_pipe t id) ~printer:print_tile

let exit_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (exit_pipe t id) ~printer:print_tile

let basic = Yojson.Basic.from_file "basic_levels.json" |> from_json

let levels_tests =
  [
    entrance_test "basic, level 1. entrance pos : (0, 0)" basic 1
      { pos = (0, 0); tile_type = () };
    exit_test "basic, level 1. exit pos : (1, 1)" basic 1
      { pos = (1, 1); tile_type = () };
    entrance_test "basic, level 2. entrance pos : (3, 4)" basic 2
      { pos = (3, 4); tile_type = () };
    exit_test "basic, level 2. exit pos : (-1, -1)" basic 2
      { pos = (-1, -1); tile_type = () };
  ]

let suite = "test suite for A2" >::: List.flatten [ levels_tests ]

let _ = run_test_tt_main suite
