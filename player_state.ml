open Yojson.Basic.Util
open Levels
open Gui

type level_id = Levels.level_id

type position = {
  x : int;
  y : int;
}

let get_x pos = pos.x

let get_y pos = pos.y

type coins = int

exception UnknownPosition of position

type p = {
  current_pos : position;
  current_level : level_id;
  coins : coins;
}

(* Implement after levels.ml is updated *)
let init_state lev lev_id = failwith "Unimplemented"

(* { current_pos = (let pipe = entrance_pipe lev lev_id in pipe.pos);
   current_level = 01; coins = 0; } *)

let get_current_level (ps : p) = ps.current_level

let get_current_pos (ps : p) = ps.current_pos

let update_pos (move : char) (p : p) =
  match move with
  | 'w' -> { x = p.current_pos.x; y = p.current_pos.y + 1 }
  | 's' -> { x = p.current_pos.x; y = p.current_pos.y - 1 }
  | 'a' -> { x = p.current_pos.x - 1; y = p.current_pos.y }
  | 'd' -> { x = p.current_pos.x + 1; y = p.current_pos.y }
  | _ -> failwith "Impossible"

(* Take char as input from gui.ml, return new state *)
let update move p =
  match move with
  | 'w' ->
      {
        current_pos = update_pos 'w' p;
        current_level = p.current_level;
        coins = p.coins;
      }
  | 's' ->
      {
        current_pos = update_pos 'w' p;
        current_level = p.current_level;
        coins = p.coins;
      }
  | 'a' ->
      {
        current_pos = update_pos 'w' p;
        current_level = p.current_level;
        coins = p.coins;
      }
  | 'd' ->
      {
        current_pos = update_pos 'w' p;
        current_level = p.current_level;
        coins = p.coins;
      }
  | _ -> failwith "Impossible"

type result =
  | Legal of p
  | Illegal
