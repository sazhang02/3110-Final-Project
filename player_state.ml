open Levels
open Board

type level_id = Levels.level_id

type coins = int

type p = {
  current_tile : tile;
  current_level : level_id;
  coins : coins; (* image : Graphics.image; *)
}

let init_state (t : Levels.t) level_id =
  {
    (* TODO: get tile next to entrance pipe*)
    current_tile = entrance_pipe t level_id;
    current_level = level_id;
    coins = 0 (* image = image; *);
  }

let get_current_level (ps : p) = ps.current_level

let get_current_tile (ps : p) = ps.current_tile

let get_current_pos (ps : p) = get_tile_coords ps.current_tile

let get_coins (ps : p) = ps.coins

(* let get_image (ps : p) = ps.image *)

let get_move move p =
  let p_x = get_x (get_current_pos p) in
  let p_y = get_y (get_current_pos p) in
  match move with
  | 'w' -> make_coord p_x (p_y + 1) (* { x = p_x; y = p_y + 1 } *)
  | 's' -> make_coord p_x (p_y - 1) (* { x = p_x; y = p_y - 1 } *)
  | 'a' -> make_coord (p_x - 1) p_y (* { x = p_x - 1; y = p_y } *)
  | 'd' -> make_coord (p_x + 1) p_y (* { x = p_x + 1; y = p_y } *)
  | _ -> raise (Failure "Unreachable")

let player_next_level p t =
  let new_level = next_level t p.current_level in
  {
    current_tile = entrance_pipe t new_level;
    current_level = new_level;
    coins = p.coins;
  }

let player_prev_level p t =
  let old_level = prev_level t p.current_level in
  (* init_state (t new_level) *)
  {
    current_tile = exit_pipe t old_level;
    current_level = old_level;
    coins = p.coins;
  }

let player_next_tile tile p t =
  {
    current_tile = tile;
    current_level = p.current_level;
    coins = p.coins;
  }

let check_tile tile p t =
  match get_tile_type tile with
  | Wall -> p
  | Pipe pipe ->
      {
        current_tile = make_tile (get_pipe_end pipe) Empty;
        (* { coords = get_pipe_end pipe; tile_type = Empty }; *)
        current_level = p.current_level;
        coins = p.coins;
      }
  | Entrance -> player_prev_level p t
  | Exit -> player_next_level p t
  | Empty -> player_next_tile tile p t

let update move p t b =
  let predicted_move = get_move move p in
  let next_tile = get_tile_c predicted_move b in
  check_tile next_tile p t

(* check_tile (get_tile_type p.current_tile) if predicted_move <>
   get_pos (exit_pipe t p.current_level) then { current_tile =
   get_tile_c predicted_move (Levels.make_board t p.current_level);
   current_level = p.current_level; coins = p.coins; } else
   player_next_level p t *)

(*type p = { current_pos : coord; current_level : level_id; coins :
  coins; (* image : Graphics.image; *) }

  let init_state (t : Levels.t) level_id = { current_pos = get_pos
  (entrance_pipe t level_id); current_level = level_id; coins = 0 (*
  image = image; *); }

  let get_current_level (ps : p) = ps.current_level

  let get_current_pos (ps : p) = getps.current_tile

  let get_coins (ps : p) = ps.coins

  (* let get_image (ps : p) = ps.image *)

  let get_move move p = match move with | 'w' -> { x = get_x
  p.current_pos; y = get_y p.current_pos + 1 } | 's' -> { x = get_x
  p.current_pos; y = get_y p.current_pos - 1 } | 'a' -> { x = get_x
  p.current_pos - 1; y = get_y p.current_pos } | 'd' -> { x = get_x
  p.current_pos + 1; y = get_y p.current_pos } | _ -> raise (Failure
  "Unreachable")

  let check_tile (tile : tile_type) = match tile with | Wall _ -> assert
  false | Pipe _ -> assert false | Entrance -> assert false | Exit ->
  assert false | Empty -> assert false

  let player_next_level p t = let new_level = next_level t
  p.current_level in { current_pos = get_pos (entrance_pipe t
  new_level); current_level = new_level; coins = p.coins; }

  let update move p t = let predicted_move = get_move move p in (* if
  check_tile (get_tile_type (get_tile p.current_pos t) *) if
  predicted_move <> get_pos (exit_pipe t p.current_level) then {
  current_pos = predicted_move; current_level = p.current_level; coins =
  p.coins; } else player_next_level p t *)

(* | 'w' -> let predicted_move = { x = get_x p.current_pos; y = get_y
   p.current_pos + 1 } in if predicted_move <> get_pos (exit_pipe t
   p.current_level) then { current_pos = predicted_move; current_level =
   p.current_level; coins = p.coins (* image = p.image; *); } else
   player_next_level p t | 's' -> let predicted_move = { x = get_x
   p.current_pos; y = get_y p.current_pos + 1 } in if predicted_move <>
   get_pos (exit_pipe t p.current_level) then { current_pos =
   predicted_move; current_level = p.current_level; coins = p.coins (*
   image = p.image; *); } else player_next_level p t | 'a' -> {
   current_pos = { x = get_x p.current_pos - 1; y = get_y p.current_pos
   }; current_level = update_level p t; coins = p.coins (* image =
   p.image; *); } | 'd' -> { current_pos = { x = get_x p.current_pos +
   1; y = get_y p.current_pos }; current_level = update_level p t; coins
   = p.coins (* image = p.image; *); } | _ -> failwith "Impossible" *)

(* type result = | Legal of p | Illegal *)
