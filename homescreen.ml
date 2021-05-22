open Graphics
open Gui

type button = {
  x : int;
  y : int;
  h : int;
  w : int;
}

let is_easy_game = ref true

let center_pos =
  let x_y = get_window_size Large in
  let x = fst x_y in
  let y = snd x_y in
  (x / 2, y / 2)

let title_img_cm = load_png "images/50/title.png"

let start_img_cm = load_png "images/50/start_game.png"

let easy_mode_selected_img_cm = load_png "images/50/easy_selected.png"

let easy_mode_unselected_img_cm =
  load_png "images/50/easy_unselected.png"

let hard_mode_selected_img_cm = load_png "images/50/hard_selected.png"

let hard_mode_unselected_img_cm =
  load_png "images/50/hard_unselected.png"

let title_pos =
  let w = img_width title_img_cm in
  make_gui_coord (fst center_pos - (w / 2)) (snd center_pos + 125)

let description =
  "Pick up all the coins to proceed to the next level and help the \
   camel get out of the desert!"

let home_instructions =
  "Choose EASY or HARD mode, and then click START GAME to begin"

let start =
  {
    x = fst center_pos - (img_width start_img_cm / 2);
    y = snd center_pos - (img_height start_img_cm / 2);
    w = img_width start_img_cm;
    h = img_height start_img_cm;
  }

let offset = 10

let easy_mode =
  {
    x = start.x;
    y = start.y - img_height easy_mode_selected_img_cm - offset;
    h = img_height easy_mode_selected_img_cm;
    w = img_width easy_mode_selected_img_cm;
  }

let hard_mode =
  {
    x = start.x + easy_mode.w + (2 * offset);
    y = start.y - img_height hard_mode_selected_img_cm - offset;
    h = img_height hard_mode_selected_img_cm;
    w = img_width hard_mode_selected_img_cm;
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

let button_is_clicked button e =
  is_in_range (e.mouse_x, e.mouse_y) button && e.button

let draw_button b = draw_rect b.x b.y b.w b.h

let rec draw_all_buttons b_lst =
  match b_lst with
  | [] -> ()
  | h :: t ->
      draw_rect h.x h.y h.w h.h;
      draw_all_buttons t

let rec init_button_imgs b_lst =
  match b_lst with
  | [] -> ()
  | h :: t ->
      let b = fst h in
      draw_at_coords (snd h) (make_gui_coord b.x b.y);
      init_button_imgs t

let easy_button_coords = make_gui_coord easy_mode.x easy_mode.y

let hard_button_coords = make_gui_coord hard_mode.x hard_mode.y

let rec choose_mode () =
  let e = wait_next_event [ Button_down; Button_up; Key_pressed ] in
  if button_is_clicked start e |> not then
    if button_is_clicked hard_mode e then (
      is_easy_game := false;
      (* print_endline (string_of_bool !is_easy_game); *)
      draw_at_coords (easy_unselected_img ()) easy_button_coords;
      draw_at_coords (hard_selected_img ()) hard_button_coords;
      choose_mode () )
    else if button_is_clicked easy_mode e then (
      is_easy_game := true;
      (* print_endline (string_of_bool !is_easy_game); *)
      draw_at_coords (hard_unselected_img ()) hard_button_coords;
      draw_at_coords (easy_selected_img ()) easy_button_coords;
      choose_mode () )
    else choose_mode ()
  else ()

let get_mode is_easy = if is_easy then easy_game else hard_game

let proceed () = get_mode !is_easy_game

let create_homescreen () =
  draw_screen_background Large;
  draw_at_coords (title_image_gc Large) title_pos;
  moveto (get_x title_pos - 50) (get_y title_pos - 25);
  draw_string description;
  moveto (get_x title_pos + 25) (get_y title_pos - 50);
  draw_string home_instructions;
  init_button_imgs
    [
      (start, start_img ());
      (easy_mode, easy_selected_img ());
      (hard_mode, hard_unselected_img ());
    ];
  draw_all_buttons [ start; hard_mode; easy_mode ];
  choose_mode ();
  proceed ()

let homescreen () =
  try create_homescreen ()
  with Graphic_failure "fatal I/O error" -> "close gui"
