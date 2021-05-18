open OUnit2
open Levels
open Board
open Player_state

(** [tile_to_string t] is the string representation of itle [t]. *)
let coords_to_string c =
  "(" ^ string_of_int (get_x c) ^ ", " ^ string_of_int (get_y c) ^ ")"

let orientation_to_string o =
  match o with
  | Left -> "left"
  | Right -> "right"
  | Up -> "up"
  | Down -> "down"

let color_to_string c =
  match c with
  | Green -> "green"
  | Red -> "red"
  | Gold -> "gold"
  | Blue -> "blue"
  | Black -> "black"

let tile_to_string tile =
  let coords = get_tile_coords tile |> coords_to_string in
  let tile_type = get_tile_type tile in
  match tile_type with
  | Wall -> "Wall @ " ^ coords
  | Pipe pipe ->
      "Pipe @ " ^ coords ^ ". End coords @ "
      ^ coords_to_string (get_pipe_end_of_tile tile)
      ^ ". Orientation "
      ^ orientation_to_string (get_tile_orientation tile)
      ^ ". Color "
      ^ color_to_string (get_pipe_color pipe)
  | Entrance _ ->
      "Entrance @ " ^ coords ^ ". Orientation "
      ^ orientation_to_string (get_tile_orientation tile)
  | Exit _ ->
      "Exit @ " ^ coords ^ ". Orientation "
      ^ orientation_to_string (get_tile_orientation tile)
  | Empty -> "Blank @ " ^ coords
  | Coin -> "Coin @ " ^ coords

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

let coin_count_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (coin_count t id) ~printer:string_of_int

let is_final_level_test name t id expected =
  name >:: fun _ ->
  assert_equal expected (is_final_level t id) ~printer:string_of_bool

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
      (make_tile (Entrance Right) (make_coord 0 1));
    exit_pipe_test "exit pipe test: basic, level 0. exit pos : (1, 0)"
      basic 0
      (make_tile (Exit Up) (make_coord 1 0));
    entrance_pipe_test
      "entr pipe test: basic, level 1. entrance pos : (3, 4)" basic 1
      (make_tile (Entrance Up) (make_coord 3 4));
    exit_pipe_test "exit pipe test: basic, level 1. exit pos : (5, 5)"
      basic 1
      (make_tile (Exit Right) (make_coord 5 5));
    entrance_pipe_test
      "entr pipe test: basic, level 2. entrance pos : (10, 10)" basic 2
      (make_tile (Entrance Right) (make_coord 10 10));
    exit_pipe_test "exit pipe test: basic, level 2. exit pos : (0, 1)"
      basic 2
      (make_tile (Exit Right) (make_coord 0 1));
    next_or_prev_level_test
      "next_level test: basic, level 0. next_level id: 1" next_level
      basic 0 1;
    invalid_test
      "next_level test: basic, level 4. next_level id: -2. Raises \
       UnknownLevel"
      next_level basic 4 (UnknownLevel (-2));
    next_or_prev_level_test
      "prev level test: basic, level 1. prev_level id: 0" prev_level
      basic 1 0;
    next_or_prev_level_test
      "prev level test: basic, level 3. prev_level id: 2" prev_level
      basic 3 2;
    invalid_test
      "prev level test: basic, level 0. prev_level raises UnknownLevel"
      prev_level basic 0 (UnknownLevel (-1));
    coin_count_test "total number of coins at level 1 is 4" basic 1 4;
    coin_count_test "total number of coins at level 0 is 2" basic 0 2;
    coin_count_test "total number of coins at level 2 is 6" basic 2 6;
    is_final_level_test "level no. 4 is the final level" basic 4 true;
    is_final_level_test "level no. 0 is not the final level" basic 0
      false;
  ]

let get_tile_test name index t expected =
  name >:: fun _ ->
  assert_equal expected (get_tile index t) ~printer:tile_to_string

let get_end_coord_test name tile expected =
  name >:: fun _ ->
  assert_equal expected
    (get_pipe_end_of_tile tile)
    ~printer:coords_to_string

let board_to_string_test name board expected =
  name >:: fun _ ->
  assert_equal expected (board_to_string board) ~printer:Fun.id

let board_tests =
  [
    (*Green Pipe tests*)
    get_end_coord_test
      "Green Right pipe at (0, 2) has end coord (14, 2)"
      (make_pipe_tile (make_coord 0 2) Green Right)
      (make_coord 14 2);
    get_end_coord_test "Green Left pipe at (15, 2) has end coord (1, 2)"
      (make_pipe_tile (make_coord 15 2) Green Left)
      (make_coord 1 2);
    get_end_coord_test "Green Up pipe at (0, 2) has end coord (0, 12)"
      (make_pipe_tile (make_coord 0 2) Green Up)
      (make_coord 0 12);
    get_end_coord_test "Green Down pipe at (2, 15) has end coord (2, 1)"
      (make_pipe_tile (make_coord 2 15) Green Down)
      (make_coord 2 1);
    (*Gold Pipe tests*)
    get_end_coord_test
      "Gold Right pipe at (0, 4) has end coord (11, 15)"
      (make_pipe_tile (make_coord 0 4) Gold Right)
      (make_coord 14 11);
    get_end_coord_test "Gold Left pipe at (15, 11) has end coord (1, 4)"
      (make_pipe_tile (make_coord 15 11) Gold Left)
      (make_coord 1 4);
    get_end_coord_test "Gold Up pipe at (9, 0) has end coord (6, 14)"
      (make_pipe_tile (make_coord 9 0) Gold Up)
      (make_coord 6 14);
    get_end_coord_test "Gold Down pipe at (6, 7) has end coord (9, 9)"
      (make_pipe_tile (make_coord 6 7) Gold Down)
      (make_coord 9 9);
    (*Red Pipe tests*)
    get_end_coord_test "Red Up pipe at (6, 0) has end coord (9, 1)"
      (make_pipe_tile (make_coord 6 0) Red Up)
      (make_coord 9 1);
    get_end_coord_test "Red Up pipe at (6, 0) has end coord (9, 1)"
      (make_pipe_tile (make_coord 4 14) Red Down)
      (make_coord 11 13);
    get_end_coord_test "Red Up pipe at (6, 0) has end coord (9, 1)"
      (make_pipe_tile (make_coord 0 4) Red Right)
      (make_coord 1 11);
    get_end_coord_test "Red Up pipe at (6, 0) has end coord (9, 1)"
      (make_pipe_tile (make_coord 9 5) Red Left)
      (make_coord 8 10);
    (*Blue Pipe tests*)
    get_end_coord_test "Blue Up pipe at (2, 1) has end coord (2, 13)"
      (make_pipe_tile (make_coord 2 1) Blue Up)
      (make_coord 2 13);
    get_end_coord_test "Blue Right"
      (make_pipe_tile (make_coord 1 4) Blue Right)
      (make_coord 4 13);
    get_end_coord_test "Blue Left"
      (make_pipe_tile (make_coord 14 9) Blue Left)
      (make_coord 9 2);
    get_end_coord_test "Blue Down"
      (make_pipe_tile (make_coord 4 14) Blue Down)
      (make_coord 13 11);
    (let board = Levels.make_board basic 4 in
     board_to_string_test "board" board "");
    (let board = Levels.make_board basic 1 in
     board_to_string_test "board" (random_item board) "");
    (* let coord = make_coord 0 0 in let entrance = make_tile coord
       Entrance in let exit = { coords = { x = 1; y = 1 }; tile_type =
       Exit } in let rooms = [] in let t = make_board entrance exit
       rooms in *)
    (*[ get_tile_test "entrance 0,0" 0 t "Entrance @ (0, 0)";
      get_tile_test "blank 1,0" 1 t "Blank @ (1, 0)"; get_tile_test
      "blank 0,1" 2 t "Blank @ (0, 1)"; get_tile_test "exit 1,1" 3 t
      "Exit @ (1, 1)"; *)
  ]

(* Player_state tests *)

let player_state_to_string (p : p) =
  "[Current tile: "
  ^ tile_to_string (get_current_tile p)
  ^ "; Current_level: "
  ^ string_of_int (get_current_level p)
  ^ "; Coins: "
  ^ string_of_int (get_coins p)
  ^ "]"

let state_tests name func t bt expected_output =
  name >:: fun _ ->
  assert_equal expected_output (func t bt)
    ~printer:player_state_to_string

let get_current_level_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_current_level state)

let get_current_tile_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (get_current_tile state)
    ~printer:tile_to_string

let get_current_pos_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_current_pos state)
    ~printer:coords_to_string

let get_coins_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_coins state) ~printer:string_of_int

let update_test name move_key p t b expected_output =
  name >:: fun _ ->
  assert_equal expected_output (update move_key p t b)
    ~printer:player_state_to_string

let example_board = Levels.make_board basic 0

(* State at position (1, 1) *)
let start_st = init_state basic example_board

(* State at position (1, 2) *)
let up_st = update 'w' start_st basic example_board

(* State at position (2, 2)*)
let middle_st = update 'd' up_st basic example_board

let player_state_tests =
  [
    (let p = make_player_state 1 1 Empty 0 0 in
     state_tests "state test: init state test" init_state basic
       example_board p);
    (let p = make_player_state 14 12 Empty 4 8 in
     state_tests "final test: final state test" final_state basic
       example_board p);
    (* current_level tests *)
    get_current_level_test
      "current level test: basic initial level is 0" start_st 0;
    (* current_tile tests *)
    get_current_tile_test
      "current tile test: basic initial tile is entrance" start_st
      (make_tile Empty (make_coord 1 1));
    get_current_tile_test
      "current tile test: middle initial tile is empty" middle_st
      (make_tile Empty (make_coord 2 2));
    (let ru_pipe = make_pipe_tile (make_coord 6 0) Red Up in
     let p = make_player_state 6 0 (get_tile_type ru_pipe) 1 0 in
     get_current_tile_test "current tile test: red pipe, up" p ru_pipe);
    (let rd_pipe = make_pipe_tile (make_coord 6 0) Red Down in
     let p = make_player_state 6 0 (get_tile_type rd_pipe) 1 0 in
     get_current_tile_test "current tile test: red pipe, down" p rd_pipe);
    (let rl_pipe = make_pipe_tile (make_coord 6 0) Red Left in
     let p = make_player_state 6 0 (get_tile_type rl_pipe) 1 0 in
     get_current_tile_test "current tile test: red pipe, left3" p
       rl_pipe);
    (let rr_pipe = make_pipe_tile (make_coord 6 0) Red Right in
     let p = make_player_state 6 0 (get_tile_type rr_pipe) 1 0 in
     get_current_tile_test "current tile test: red pipe, right" p
       rr_pipe);
    (let bu_pipe = make_pipe_tile (make_coord 2 1) Blue Up in
     let p = make_player_state 2 1 (get_tile_type bu_pipe) 3 0 in
     get_current_tile_test "current tile test: blue pipe, up" p bu_pipe);
    (let bd_pipe = make_pipe_tile (make_coord 2 1) Blue Down in
     let p = make_player_state 2 1 (get_tile_type bd_pipe) 3 0 in
     get_current_tile_test "current tile test: blue pipe, down" p
       bd_pipe);
    (let bl_pipe = make_pipe_tile (make_coord 2 1) Blue Left in
     let p = make_player_state 2 1 (get_tile_type bl_pipe) 3 0 in
     get_current_tile_test "current tile test: blue pipe, left" p
       bl_pipe);
    (let br_pipe = make_pipe_tile (make_coord 2 1) Blue Right in
     let p = make_player_state 2 1 (get_tile_type br_pipe) 3 0 in
     get_current_tile_test "current tile test: blue pipe, right" p
       br_pipe);
    (let gu_pipe = make_pipe_tile (make_coord 2 1) Green Up in
     let p = make_player_state 2 1 (get_tile_type gu_pipe) 3 0 in
     get_current_tile_test "current tile test: green pipe, up" p gu_pipe);
    (let gd_pipe = make_pipe_tile (make_coord 2 1) Green Down in
     let p = make_player_state 2 1 (get_tile_type gd_pipe) 3 0 in
     get_current_tile_test "current tile test: green pipe, down" p
       gd_pipe);
    (let gl_pipe = make_pipe_tile (make_coord 2 1) Green Left in
     let p = make_player_state 2 1 (get_tile_type gl_pipe) 3 0 in
     get_current_tile_test "current tile test: green pipe, left" p
       gl_pipe);
    (let gr_pipe = make_pipe_tile (make_coord 2 1) Green Right in
     let p = make_player_state 2 1 (get_tile_type gr_pipe) 3 0 in
     get_current_tile_test "current tile test: green pipe, right" p
       gr_pipe);
    (let goldu_pipe = make_pipe_tile (make_coord 3 4) Gold Up in
     let p = make_player_state 3 4 (get_tile_type goldu_pipe) 2 0 in
     get_current_tile_test "current tile test: gold pipe, up" p
       goldu_pipe);
    (let goldd_pipe = make_pipe_tile (make_coord 3 4) Gold Down in
     let p = make_player_state 3 4 (get_tile_type goldd_pipe) 2 0 in
     get_current_tile_test "current tile test: gold pipe, down" p
       goldd_pipe);
    (let goldl_pipe = make_pipe_tile (make_coord 3 4) Gold Left in
     let p = make_player_state 3 4 (get_tile_type goldl_pipe) 2 0 in
     get_current_tile_test "current tile test: gold pipe, left" p
       goldl_pipe);
    (let goldr_pipe = make_pipe_tile (make_coord 3 4) Gold Right in
     let p = make_player_state 3 4 (get_tile_type goldr_pipe) 2 0 in
     get_current_tile_test "current tile test: gold pipe, right" p
       goldr_pipe);
    (let blacku_pipe = make_pipe_tile (make_coord 2 2) Black Up in
     let p = make_player_state 2 2 (get_tile_type blacku_pipe) 1 0 in
     get_current_tile_test "current tile test: black pipe, up" p
       blacku_pipe);
    (let blackd_pipe = make_pipe_tile (make_coord 2 2) Black Down in
     let p = make_player_state 2 2 (get_tile_type blackd_pipe) 1 0 in
     get_current_tile_test "current tile test: black pipe, down" p
       blackd_pipe);
    (let blackl_pipe = make_pipe_tile (make_coord 2 2) Black Left in
     let p = make_player_state 2 2 (get_tile_type blackl_pipe) 1 0 in
     get_current_tile_test "current tile test: black pipe, left" p
       blackl_pipe);
    (let blackr_pipe = make_pipe_tile (make_coord 2 2) Black Right in
     let p = make_player_state 2 2 (get_tile_type blackr_pipe) 1 0 in
     get_current_tile_test "current tile test: black pipe, right" p
       blackr_pipe);
    (let entrance = make_tile (Entrance Left) (make_coord 15 12) in
     let p = make_player_state 15 12 (get_tile_type entrance) 4 0 in
     get_current_tile_test
       "current tile test: entrance pipe, down in level 4" p entrance);
    (let coin_tile = make_tile Coin (make_coord 4 2) in
     let p = make_player_state 4 2 (get_tile_type coin_tile) 4 1 in
     get_current_tile_test "current tile test: coin tile, level 4" p
       coin_tile);
    (let exit = make_tile (Exit Left) (make_coord 5 9) in
     let p = make_player_state 5 9 (get_tile_type exit) 4 0 in
     get_current_tile_test "current tile test: exit tile, level 4" p
       exit);
    (let wall = make_tile Wall (make_coord 1 1) in
     let p = make_player_state 1 1 (get_tile_type wall) 4 0 in
     get_current_tile_test "current tile test: wall tile, level 4" p
       wall);
    (* current_position tests *)
    get_current_pos_test
      "current position test: basic initial position is (1, 1)" start_st
      (make_coord 1 1);
    get_current_pos_test
      "current position test: middle initial position is (2, 2)"
      middle_st (make_coord 2 2);
    (* coins test *)
    get_coins_test "coin test: basic initial coin count is 0" start_st 0;
    (let p = make_player_state 9 10 Empty 1 2 in
     let p' = update 'd' p basic example_board in
     get_coins_test "coin test: coin count + 1" p' 3);
    (* update tests *)
    (let p = make_player_state 2 3 Empty 0 0 in
     update_test "update test: move up to empty tile" 'w' middle_st
       basic example_board p);
    (let p = make_player_state 1 2 Empty 0 0 in
     update_test "update test: move left to empty tile" 'a' middle_st
       basic example_board p);
    (let p = make_player_state 2 1 Empty 0 0 in
     update_test "update test: move down to empty tile" 's' middle_st
       basic example_board p);
    (let p = make_player_state 3 2 Empty 0 0 in
     update_test "update test: move right to empty tile" 'd' middle_st
       basic example_board p);
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [ (* board_tests; *) levels_tests; player_state_tests ]

let _ = run_test_tt_main suite
