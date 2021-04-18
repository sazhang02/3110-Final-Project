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

let update move p =
  match move with
  | 'w' ->
      (* { p with current_pos = { x = p.current_pos; y = p.current_pos +
         1 }} *)
      (* { p with current_pos = {{p.current_pos with y = get_y
         p.current_pos}} } *)
      (* { p with current_pos = {x = get_x p.current_pos; y = get_y
         p.current_pos + 1 }} *)
      {
        current_pos =
          { x = get_x p.current_pos; y = get_y p.current_pos + 1 };
        current_level = p.current_level;
        coins = p.coins (* image = p.image; *);
      }
      (*{e with f1 = e1; ...; fn = en}*)
  | 's' ->
      {
        current_pos =
          { x = get_x p.current_pos; y = get_y p.current_pos - 1 };
        current_level = p.current_level;
        coins = p.coins (* image = p.image; *);
      }
      (* { p with current_pos = {{p.current_pos with y = get_y
         p.current_pos}} } *)
  | 'a' ->
      {
        current_pos =
          { x = get_x p.current_pos - 1; y = get_y p.current_pos };
        current_level = p.current_level;
        coins = p.coins (* image = p.image; *);
      }
  | 'd' ->
      {
        current_pos =
          { x = get_x p.current_pos + 1; y = get_y p.current_pos };
        current_level = p.current_level;
        coins = p.coins (* image = p.image; *);
      }
  | _ -> failwith "Impossible"

(* type result = | Legal of p | Illegal *)
