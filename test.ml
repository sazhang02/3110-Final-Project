open OUnit2
open Levels
open Board

(** [tile_to_string t] is the string representation of itle [t]. *)
open Player_state

let coords_to_string c =
  "(" ^ string_of_int (get_x c) ^ ", " ^ string_of_int (get_y c) ^ ")"

let orientation_to_string o =
  match o with
  | Left -> "left"
  | Right -> "right"
  | Up -> "up"
  | Down -> "down"

let color_to_string c =
  match c with Green -> "green" | Red -> "red" | Blue -> "blue"

let tile_to_string tile =
  let coords = get_tile_coords tile |> coords_to_string in
  let tile_type = get_tile_type tile in
  match tile_type with
  | Wall -> "Wall @ " ^ coords
  | Pipe pipe ->
      "Pipe @ " ^ coords ^ ". End coords @ "
      ^ coords_to_string (get_pipe_end pipe)
      ^ ". Orientation "
      ^ orientation_to_string (get_pipe_orientation pipe)
      ^ ". Color "
      ^ color_to_string (get_pipe_color pipe)
  | Entrance -> "Entrance @ ("
  | Exit -> "Exit @ ("
  | Empty -> "Blank @ ("

let entrance_pipe_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (entrance_pipe t id) ~printer:tile_to_string

let exit_pipe_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (exit_pipe t id) ~printer:tile_to_string

let invalid_test name f t id exn =
  name >:: fun _ -> assert_raises exn (fun () -> f t id)

let next_or_prev_level_test name f t id expected =
  name >:: fun _ ->
  assert_equal expected (f t id) ~printer:string_of_int

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let basic = Yojson.Basic.from_file "basic_levels.json" |> from_json

let levels_tests =
  [
    (*entrance/exit pipe tests*)
    entrance_pipe_test
      "entr pipe test: basic, level 0. entrance pos : (0, 1)" basic 0
      (make_tile (make_coord 0 1) Entrance);
    (* { coords = { x = 0; y = 1 }; tile_type = Entrance }; *)
    exit_pipe_test "exit pipe test: basic, level 0. exit pos : (1, 0)"
      basic 0
      (make_tile (make_coord 1 0) Exit);
    entrance_pipe_test
      "entr pipe test: basic, level 1. entrance pos : (3, 4)" basic 1
      (make_tile (make_coord 3 4) Entrance);
    (* { coords = { x = 3; y = 4 }; tile_type = Entrance }; *)
    (* invalid_test "exit pipe test: basic, level 1. exit pos : (-1, -1)
       raises \ InvalidTile" exit_pipe basic 1 (InvalidTile { x = -1; y
       = -1 }); *)
    (*next/prev level tests*)
    next_or_prev_level_test
      "next_level test: basic, level 0. next_level id: 1" next_level
      basic 0 1;
    invalid_test
      "next_level test: basic, level 2. next_level id: -1. Raises \
       UnknownLevel"
      next_level basic 2 (UnknownLevel (-1));
    next_or_prev_level_test
      "prev level test: basic, level 1. prev_level id: 0" prev_level
      basic 1 0;
    invalid_test
      "prev level test: basic, level 0. prev_level raises UnknownLevel"
      prev_level basic 0 (UnknownLevel (-1));
  ]

let get_tile_test name index t expected =
  name >:: fun _ ->
  assert_equal expected (tile_to_string (get_tile index t))

let make_tile_pair_test name entrance color orientation expected =
  name >:: fun _ ->
  assert_equal expected
    (make_pipe_tile_pair entrance color orientation)
    ~printer:(pp_list tile_to_string)

let board_tests =
  (* let entrance = { coords = { x = 0; y = 0 }; tile_type = Entrance }
     in let exit = { coords = { x = 1; y = 1 }; tile_type = Exit } in
     let rooms = [] in let t = make_board entrance exit rooms in *)
  [ (* get_tile_test "entrance 0,0" 0 t "Entrance @ (0, 0)";
       get_tile_test "blank 1,0" 1 t "Blank @ (1, 0)"; get_tile_test
       "blank 0,1" 2 t "Blank @ (0, 1)"; get_tile_test "exit 1,1" 3 t
       "Exit @ (1, 1)"; *)
    (* make_tile_pair_test "green pipe facing right at (0, 1),green pipe
       facing left at \ (15, 1)" { x = 0; y = 1 } Green Right [ { coords
       = { x = 0; y = 1 }; tile_type = Pipe { end_coords = { x = 14; y =
       1 }; orientation = Right; color = Green; }; }; { coords = { x =
       15; y = 1 }; tile_type = Pipe { end_coords = { x = 1; y = 1 };
       orientation = Left; color = Green; }; }; ]; *) ]

let start_st = init_state basic 0

let right_st = update 'd' start_st

let get_current_level_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_current_level state)

let get_current_pos_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_current_pos state)
    ~printer:coords_to_string

let get_coins name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_coins state) ~printer:string_of_int

let player_state_tests =
  [ (* get_current_level_test "basic initial level is 0" start_st 0;
       get_current_pos_test "basic initial pos is (0,1)" start_st { x =
       0; y = 1 }; get_current_pos_test "basic pos to right is (1,0)"
       start_st { x = 1; y = 0 }; get_coins "basic initial coins is 0"
       start_st 0; *) ]

let suite =
  "test suite for A2"
  >::: List.flatten [ levels_tests; board_tests; player_state_tests ]

let _ = run_test_tt_main suite
