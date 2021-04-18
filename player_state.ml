open Levels
open Board

type level_id = Levels.level_id

type coins = int

type p = {
  current_pos : coord;
  current_level : level_id;
  coins : coins; (* image : Graphics.image; *)
}

let init_state (t : Levels.t) level_id =
  {
    current_pos = get_pos (entrance_pipe t level_id);
    current_level = level_id;
    coins = 0 (* image = image; *);
  }

let get_current_level (ps : p) = ps.current_level

let get_current_pos (ps : p) = ps.current_pos

let get_coins (ps : p) = ps.coins

(* let get_image (ps : p) = ps.image *)

let get_move move p =
  match move with
  | 'w' -> { x = get_x p.current_pos; y = get_y p.current_pos + 1 }
  | 's' -> { x = get_x p.current_pos; y = get_y p.current_pos - 1 }
  | 'a' -> { x = get_x p.current_pos - 1; y = get_y p.current_pos }
  | 'd' -> { x = get_x p.current_pos + 1; y = get_y p.current_pos }
  | _ -> raise (Failure "Unreachable")

let player_next_level p t =
  let new_level = next_level t p.current_level in
  {
    current_pos = get_pos (entrance_pipe t new_level);
    current_level = new_level;
    coins = p.coins;
  }

let update move p t =
  let predicted_move = get_move move p in
  if predicted_move <> get_pos (exit_pipe t p.current_level) then
    {
      current_pos = predicted_move;
      current_level = p.current_level;
      coins = p.coins;
    }
  else player_next_level p t

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
