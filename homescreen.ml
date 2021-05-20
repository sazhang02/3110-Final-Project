open Graphics
open Gui

(* let homescreen () : string * Levels.level_id = failwith "" *)

type button = {
  x : int;
  y : int;
  h : int;
  w : int;
}

let init = ("playtest_levels.json", 0)

let is_easy_game = ref true

let center_pos =
  let x_y = get_window_size Large in
  let x = fst x_y in
  let y = snd x_y in
  (x / 2, y / 2)

let start_img_w_h = (300, 100)

let start =
  {
    x = fst center_pos - (fst start_img_w_h / 2);
    y = snd center_pos - (snd start_img_w_h / 2);
    h = snd start_img_w_h;
    w = fst start_img_w_h;
  }

let offset = 10

let easy_mode =
  {
    x = start.x;
    y = start.y - 100;
    h = start.h / 2;
    w = (start.w / 2) - offset;
  }

let hard_mode =
  {
    x = start.x + easy_mode.w + (2 * offset);
    y = start.y - 100;
    h = start.h / 2;
    w = (start.w / 2) - offset;
  }

let easy_game = "playtest_levels.json"

let hard_game = "game_levels.json"

let is_in_range pos button =
  let x = fst pos in
  let y = snd pos in
  x <= button.x + button.w
  && x >= button.x
  && y <= button.y + button.h
  && y >= button.y

(*utop code #require "graphics";; open Graphics;; open_graph "
  1100x800";; *)
let button_is_clicked button e =
  is_in_range (e.mouse_x, e.mouse_y) button && e.button

let draw_button b = draw_rect b.x b.y b.w b.h

let rec draw_all_buttons b_lst =
  match b_lst with
  | [] -> ()
  | h :: t ->
      draw_rect h.x h.y h.w h.h;
      draw_all_buttons t

let rec choose_mode () =
  let e = wait_next_event [ Button_down; Button_up; Key_pressed ] in
  if button_is_clicked start e |> not then
    if button_is_clicked hard_mode e then (
      is_easy_game := false;
      print_endline (string_of_bool !is_easy_game);
      choose_mode ())
    else if button_is_clicked easy_mode e then (
      is_easy_game := true;
      print_endline (string_of_bool !is_easy_game);
      choose_mode ())
    else choose_mode ()
  else ()

let get_mode is_easy =
  (* print_endline ("bonk" ^ string_of_bool !is_easy_game); *)
  if is_easy then easy_game else hard_game

let proceed () =
  print_endline ("is easy? " ^ string_of_bool !is_easy_game);
  print_endline ("mode " ^ get_mode !is_easy_game);
  (get_mode !is_easy_game, 0)

let homescreen () =
  (* open_graph ""; *)
  draw_button start;

  draw_all_buttons [ start; hard_mode; easy_mode ];
  choose_mode ();
  proceed ()
