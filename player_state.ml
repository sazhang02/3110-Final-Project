open Yojson.Basic.Util
open Levels
open Gui

type level_id = Levels.level_id

type position = Gui.coords

type coins = int

exception UnknownPosition of position

type p = {
  current_pos : position;
  current_level : level_id;
  coins : coins;
}

let init_state lev lev_id = entrance_pipe lev lev_id

let get_current_level (ps : p) = ps.current_level

let get_current_pos (ps : p) = ps.current_pos

let update_pos (move : char) (p : p) =
  match move with
  | 'w' -> { x = get_x p.current_pos; y = get_y p.current_pos + 1 }
  | 's' -> { x = p.current_pos.x; y = get_y p.current_pos - 1 }
  | 'a' -> { x = get_x p.current_pos - 1; y = get_y p.current_pos }
  | 'd' -> { x = get_x p.current_pos + 1; y = get_y p.current_pos }
  | _ -> failwith "Not a valid move."

(* Take char as input from gui.ml, return new state *)
let update p =
  match Graphics.read_key () with
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
  | _ -> failwith "Not a valid move."

type result =
  | Legal of p
  | Illegal
