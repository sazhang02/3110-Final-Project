open Levels
open Board
open Boss_state

type level_id = Levels.level_id

type move = {
  pos : coord;
  action : orientation;
}

let get_move_pos move = move.pos

let get_move_action move = move.action

type p = {
  current_tile : tile;
  current_level : level_id;
  coins : int;
  steps : int;
}

let get_current_level (p : p) = p.current_level

let get_current_tile (p : p) = p.current_tile

let get_current_pos (p : p) = get_tile_coords p.current_tile

let get_coins (p : p) = p.coins

let get_steps (p : p) = p.steps

let offset_player (t : Levels.t) (bt : Board.t) f (level_id : level_id)
    =
  let pipe = f t level_id in
  let pipe_coords = get_tile_coords pipe in
  let e_x = get_x pipe_coords in
  let e_y = get_y pipe_coords in
  match get_tile_orientation pipe with
  | Left -> make_coord (e_x - 1) e_y
  | Right -> make_coord (e_x + 1) e_y
  | Up -> make_coord e_x (e_y + 1)
  | Down -> make_coord e_x (e_y - 1)

let init_state (t : Levels.t) (bt : Board.t) =
  {
    current_tile = get_tile_c (offset_player t bt entrance_pipe 0) bt;
    current_level = 0;
    coins = 0;
    steps = 0;
  }

let final_state (t : Levels.t) (bt : Board.t) steps =
  let final_level_id = Levels.final_level_id t in
  {
    current_tile =
      get_tile_c (offset_player t bt entrance_pipe final_level_id) bt;
    current_level = final_level_id;
    coins = coin_count t (final_level_id - 1);
    steps;
  }

let check_coin (tile : tile) p t =
  let coins =
    if get_tile_type tile = Coin then p.coins + 1 else p.coins
  in
  let current_tile = make_tile Empty (get_tile_coords tile) in
  {
    current_tile;
    current_level = p.current_level;
    coins;
    steps = p.steps + 1;
  }

let check_current_coin_tile current_pos p bt t =
  let tile = get_tile_c current_pos bt in
  match get_tile_type tile with
  | Coin ->
      let p' = check_coin tile p t in
      get_coins p'
  | _ -> p.coins

let new_level_state p (t : Levels.t) (bt : Board.t) f level_id =
  let current_tile = get_tile_c (offset_player t bt f level_id) bt in
  let coin_count =
    check_current_coin_tile (get_tile_coords current_tile) p bt t
  in
  {
    current_tile;
    current_level = level_id;
    coins = coin_count;
    steps = p.steps + 1;
  }

(* Helper functions for update *)
let get_move move_key p =
  let p_x = get_x (get_current_pos p) in
  let p_y = get_y (get_current_pos p) in
  match move_key with
  | 'w' -> { pos = make_coord p_x (p_y + 1); action = Up }
  | 's' -> { pos = make_coord p_x (p_y - 1); action = Down }
  | 'a' -> { pos = make_coord (p_x - 1) p_y; action = Left }
  | 'd' -> { pos = make_coord (p_x + 1) p_y; action = Right }
  | _ -> raise (Failure "Unreachable")

let check_orientation (tile : Board.tile) move (p : p) =
  match get_tile_orientation tile with
  | Left -> if move.action == Right then true else false
  | Right -> if move.action == Left then true else false
  | Up -> if move.action == Down then true else false
  | Down -> if move.action == Up then true else false

let player_prev_level p t b =
  let old_level = prev_level t p.current_level in
  new_level_state p t b exit_pipe old_level

let player_next_level p t b =
  let new_level = next_level t p.current_level in
  new_level_state p t b entrance_pipe new_level

let player_enter_pipe (pipe : Board.tile) move p t bt =
  let current_pos = get_pipe_end_of_tile pipe in
  let coin_count = check_current_coin_tile current_pos p bt t in
  {
    current_tile = make_tile Empty current_pos;
    current_level = p.current_level;
    coins = coin_count;
    steps = p.steps + 1;
  }

let player_next_tile tile p t =
  {
    current_tile = tile;
    current_level = p.current_level;
    coins = p.coins;
    steps = p.steps + 1;
  }

let check_tile tile p t b move =
  match get_tile_type tile with
  | Wall -> p
  | Pipe pipe ->
      if check_orientation tile move p then
        player_enter_pipe tile move p t b
      else p
  | Entrance _ ->
      if is_final_level t p.current_level then p
      else if check_orientation tile move p then player_prev_level p t b
      else p
  | Exit _ ->
      if p.coins < coin_count t p.current_level then p
      else if check_orientation tile move p then player_next_level p t b
      else p
  | Coin -> check_coin tile p t
  (* | Coin collected -> check_coin tile collected p t *)
  | Empty -> player_next_tile tile p t
  | Item _ ->
      failwith "Cannot get items in levels other than final level."

let final_enter_pipe tile move p b_state b_state' t bt =
  if check_orientation tile move p then
    (player_enter_pipe tile move p t bt, b_state')
  else (p, b_state)

let final_enter_entrance tile p t bt b_state b_state' move =
  if is_final_level t p.current_level then (p, b_state)
  else if check_orientation tile move p then
    (player_prev_level p t bt, b_state')
  else (p, b_state)

let final_enter_exit tile p t bt b_state b_state' move =
  if p.coins < coin_count t p.current_level then (p, b_state)
  else if check_orientation tile move p then
    (player_next_level p t bt, b_state')
  else (p, b_state)

let check_item
    (item : item)
    (tile : tile)
    (p : p)
    (bt : Board.t)
    (b_state : Boss_state.b) =
  let amt_damage = match item with Damage -> 25 | Bomb -> 50 in
  let b_state' = decrease_health b_state amt_damage in
  let current_tile = make_tile Empty (get_tile_coords tile) in
  let p' =
    {
      current_tile;
      current_level = p.current_level;
      coins = p.coins;
      steps = p.steps + 1;
    }
  in
  (p', b_state')

let check_final_level_tile tile p t bt b_state b_state' move =
  match get_tile_type tile with
  | Wall -> (p, b_state)
  | Pipe pipe -> final_enter_pipe tile move p b_state b_state' t bt
  | Entrance _ -> final_enter_entrance tile p t bt b_state b_state' move
  | Exit _ -> final_enter_exit tile p t bt b_state b_state' move
  | Coin ->
      let p' = check_coin tile p t in
      (p', b_state')
  | Empty ->
      let p = player_next_tile tile p t in
      (p, b_state')
  | Item item -> check_item item tile p bt b_state'

let update (move_key : char) p (t : Levels.t) (b : Board.t) =
  let predicted_move = get_move move_key p in
  let next_tile = get_tile_c predicted_move.pos b in
  check_tile next_tile p t b predicted_move

let check_collision move (b_state : Boss_state.b) =
  let p_pos = get_move_pos move in
  let b_pos = Boss_state.get_current_pos b_state in
  if get_x p_pos = get_x b_pos && get_y p_pos = get_y b_pos then true
  else false

let final_level_update
    (move_key : char)
    p
    (t : Levels.t)
    (bt : Board.t)
    (b_state : Boss_state.b) =
  let predicted_move = get_move move_key p in
  let next_tile = get_tile_c predicted_move.pos bt in
  let b_state' = move_boss predicted_move.pos b_state bt in
  if
    check_collision predicted_move b_state
    || check_collision predicted_move b_state'
  then
    let entrance_pos =
      get_current_pos (final_state t bt (p.steps + 1))
    in
    ( final_state t bt (p.steps + 1),
      Boss_state.init_state entrance_pos bt )
  else
    check_final_level_tile next_tile p t bt b_state b_state'
      predicted_move

(* let boss_move = p = p' in if boss_move then let b_state' = move_boss
   (get_pos p.current_tile) b_state bt in (p', b_state') *)
(* else (p, b_state) *)

let make_player_state x y tile_type level coins steps =
  let tile = make_tile tile_type (make_coord x y) in
  { current_tile = tile; current_level = level; coins; steps }
