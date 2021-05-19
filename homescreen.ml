open Graphics

(* let homescreen () : string * Levels.level_id = failwith "" *)
type button_type =
  | Start
  | Id of Levels.level_id
  | File of string

type button = {
  x : int;
  y : int;
  h : int;
  w : int;
  selected : bool;
  typ : button_type;
}

let init = ("playtest_levels.json", 0)

(* true is easy, false is hard *)
type game_state = { m : bool }

let current_game_state = ref { m = true }

let level_0 =
  { x = 400; y = 400; h = 70; w = 70; selected = false; typ = Id 0 }

let hard_mode =
  ref
    {
      x = 600;
      y = 600;
      h = 70;
      w = 70;
      selected = false;
      typ = File "game_levels.json";
    }

let start =
  { x = 200; y = 200; h = 70; w = 100; selected = false; typ = Start }

let file_name = "playtest_levels.json"

let is_in_range pos button =
  let x = fst pos in
  let y = snd pos in
  x <= button.x + button.w
  && x >= button.x
  && y <= button.y + button.h
  && y >= button.y

(*utop code #require "graphics";; open Graphics;; open_graph "
  1100x800";; *)
let click_button button e =
  (* if is_in_range (e.mouse_x, e.mouse_y) button && e.button then {
     button with selected = not button.selected } else button *)
  is_in_range (e.mouse_x, e.mouse_y) button && e.button

let draw_button b = draw_rect b.x b.y b.w b.h

let rec draw_all_buttons b_lst =
  match b_lst with
  | [] -> ()
  | h :: t ->
      draw_rect h.x h.y h.w h.h;
      draw_all_buttons t

(*looping button, use for toggles i think*)
let rec toggle () button =
  let e = wait_next_event [ Button_down; Button_up; Key_pressed ] in
  let bu =
    if click_button button e then
      { button with selected = not button.selected }
    else button
  in
  print_endline (string_of_bool bu.selected);
  if bu.selected then (
    (* match bu.typ with | File f -> (f, snd init) | Id i -> (fst init,
       i) | Start -> init *)
    clear_graph ();
    draw_button bu;
    draw_circle 100 100 100 )
  else (
    (* init *)
    clear_graph ();
    draw_button bu;
    draw_rect 100 100 100 100 );
  if (not bu.selected) && not (bu.typ = Start) then toggle () bu else ()

let rec toggle2 () button st =
  let e = wait_next_event [ Button_down; Button_up ] in
  let bu =
    if click_button button e then
      { button with selected = not button.selected }
    else button
  in
  let st' =
    match bu.typ with
    | File f -> toggle2 () bu (f, snd st)
    | Id i -> toggle2 () bu (fst st, i)
    | Start -> st
  in
  print_endline (fst st' ^ string_of_int (snd st'));
  st'

(* if (not bu.selected) && not (bu.typ = Start) then toggle2 () bu else
   () *)

(* let st' = if bu.selected then match bu.typ with | File f -> toggle2
   () bu (f, snd st) | Id i -> toggle2 () bu (fst st, i) | Start -> st
   else st in print_endline (fst st' ^ string_of_int (snd st')); st' *)

let rec proceed () =
  let e = wait_next_event [ Button_down; Button_up ] in
  if click_button start e then (file_name, 0) else proceed ()

let homescreen () =
  (* open_graph ""; *)
  draw_button start;

  (* draw_all_buttons [ start; !hard_mode ]; *)
  proceed ()

(* toggle () hard_mode; *)
(* toggle2 () !hard_mode init *)

(* clear_graph (); *)

(* draw_button b; loop () b; *)
(* close_graph () *)
