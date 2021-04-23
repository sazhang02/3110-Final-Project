open OUnit2
open Levels
open Board
open Player_state

(** TODO: add tile_type to printing*)
let print_tile (tile : tile) =
  let pos =
    String.concat ","
      [ string_of_int tile.coords.x; string_of_int tile.coords.y ]
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
      { coords = { x = 0; y = 1 }; tile_type = Entrance };
    exit_pipe_test "exit pipe test: basic, level 0. exit pos : (1, 0)"
      basic 0
      { coords = { x = 1; y = 0 }; tile_type = Exit };
    entrance_pipe_test
      "entr pipe test: basic, level 1. entrance pos : (3, 4)" basic 1
      { coords = { x = 3; y = 4 }; tile_type = Entrance };
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

(** [tile_to_string t] is the string representation of itle [t]. *)
let tile_to_string tile =
  match tile with
  | { coords = c; tile_type = t } ->
      begin
        match t with
        | Wall -> "Wall @ ("
        | Pipe _ -> "Pipe @\n   ("
        | Entrance -> "Entrance @ ("
        | Exit -> "Exit @ ("
        | Empty -> "Blank @ ("
      end
      ^ string_of_int c.x ^ ", " ^ string_of_int c.y ^ ")"

let get_tile_test name index t expected =
  name >:: fun _ ->
  assert_equal expected (tile_to_string (get_tile index t))

(* let board_tests = let entrance = { coords = { x = 0; y = 0 };
   tile_type = Entrance } in let exit = { coords = { x = 1; y = 1 };
   tile_type = Exit } in let t = make_board 2 2 entrance exit in [
   get_tile_test "entrance 0,0" 0 t "Entrance @ (0, 0)"; get_tile_test
   "blank 1,0" 1 t "Blank @ (1, 0)"; get_tile_test "blank 0,1" 2 t
   "Blank @ (0, 1)"; get_tile_test "exit 1,1" 3 t "Exit @ (1, 1)"; ] *)

let start_st = init_state basic 0

let right_st = update 'd' start_st

let string_of_coords coords =
  match coords with
  | { x = v1; y = v2 } -> string_of_int v1 ^ ", " ^ string_of_int v2

let get_current_level_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_current_level state)

let get_current_pos_test name (state : p) expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_current_pos state)
    ~printer:string_of_coords

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
  >::: List.flatten [ levels_tests; player_state_tests ]

let _ = run_test_tt_main suite
