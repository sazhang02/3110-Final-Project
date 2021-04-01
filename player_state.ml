open Levels
open Gui

type level_id = Levels.level_id

type coins = int

type p = {
  current_pos : Gui.coords;
  current_level : level_id;
  coins : coins; (* image : Graphics.image; *)
}

let init_state (t : Levels.t) level_id =
  {
    current_pos = board_to_gui (get_pos (entrance_pipe t level_id));
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
      {
        current_pos =
          { x = get_x p.current_pos; y = get_y p.current_pos + 1 };
        current_level = p.current_level;
        coins = p.coins (* image = p.image; *);
      }
  | 's' ->
      {
        current_pos =
          { x = get_x p.current_pos; y = get_y p.current_pos - 1 };
        current_level = p.current_level;
        coins = p.coins (* image = p.image; *);
      }
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

type result =
  | Legal of p
  | Illegal
