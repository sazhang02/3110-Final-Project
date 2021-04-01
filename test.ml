open OUnit2
open Levels

(* open Player_state *)

(** TODO: add tile_type to printing*)
let print_tile (tile : tile) =
  let pos =
    String.concat ","
      [ string_of_int tile.pos.x; string_of_int tile.pos.y ]
  in
  String.concat "" [ "("; pos; ")" ]

let entrance_pipe_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (entrance_pipe t id) ~printer:print_tile

let exit_pipe_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (exit_pipe t id) ~printer:print_tile

let invalid_test name f t id exn =
  name >:: fun _ -> assert_raises exn (fun () -> f t id)

let next_or_prev_level_test name f t id expected =
  name >:: fun _ ->
  assert_equal expected (f t id) ~printer:string_of_int

let basic = Yojson.Basic.from_file "basic_levels.json" |> from_json

let levels_tests =
  [
    (*entrance/exit pipe tests*)
    entrance_pipe_test
      "entr pipe test: basic, level 0. entrance pos : (0, 0)" basic 0
      { pos = { x = 0; y = 0 }; tile_type = () };
    exit_pipe_test "exit pipe test: basic, level 0. exit pos : (1, 1)"
      basic 0
      { pos = { x = 1; y = 1 }; tile_type = () };
    entrance_pipe_test
      "entr pipe test: basic, level 1. entrance pos : (3, 4)" basic 1
      { pos = { x = 3; y = 4 }; tile_type = () };
    invalid_test
      "exit pipe test: basic, level 1. exit pos : (-1, -1) raises \
       InvalidTile"
      exit_pipe basic 1
      (InvalidTile { x = -1; y = -1 });
    (*next/prev level tests*)
    next_or_prev_level_test
      "next_level test: basic, level 0. next_level id: 1" next_level
      basic 0 1;
    invalid_test
      "next_level test: basic, level 1. next_level id: -1. Raises \
       UnknownLevel"
      next_level basic 1 (UnknownLevel (-1));
    next_or_prev_level_test
      "prev level test: basic, level 1. prev_level id: 0" prev_level
      basic 1 0;
    invalid_test
      "prev level test: basic, level 0. prev_level raises UnknownLevel"
      prev_level basic 0 (UnknownLevel (-1));
  ]

let suite = "test suite for A2" >::: List.flatten [ levels_tests ]

let _ = run_test_tt_main suite
