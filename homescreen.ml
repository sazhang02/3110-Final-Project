open Graphics

(* let homescreen () : string * Levels.level_id = failwith "" *)
type button_type =
  | Start
  | Toggle

type button = {
  x : int;
  y : int;
  h : int;
  w : int;
  selected : bool;
  typ : button_type;
}

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

let b =
  { x = 400; y = 400; h = 70; w = 70; selected = false; typ = Toggle }

let start =
  { x = 200; y = 200; h = 70; w = 100; selected = false; typ = Start }

let file_name = "playtest_levels.json"

let proceed () =
  let e = wait_next_event [ Button_down; Button_up ] in
  if click_button start e then (file_name, 0) else failwith "idk man"

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
    clear_graph ();
    draw_button bu;
    draw_circle 100 100 100 )
  else (
    clear_graph ();
    draw_button bu;
    draw_rect 100 100 100 100 );
  if e.key <> 'q' then toggle () bu else ()

let homescreen () =
  (* open_graph ""; *)
  draw_button start;
  (* clear_graph (); *)
  proceed ()

(* draw_button b; loop () b; *)
(* close_graph () *)
