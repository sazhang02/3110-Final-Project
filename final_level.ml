open Graphics
open Gui

let zoom = ref Large

let rec get_input player player_img b : unit =
  match read_key () with
  | 'q' -> close_graph ()
  | 'p' ->
      let resized_info = increase_zoom player player_img !zoom b in
      let resized_player = snd resized_info in
      let new_zoom_size = fst resized_info in
      zoom := new_zoom_size;
      get_input player resized_player b
  | 'm' ->
      let resized_info = decrease_zoom player player_img !zoom b in
      let resized_player = snd resized_info in
      let new_zoom_size = fst resized_info in
      zoom := new_zoom_size;
      get_input player resized_player b
  | _ -> get_input player player_img b

let final_level (b : Board.t) (z : Gui.scaling) (p : Player_state.p) =
  print_endline "at final level";
  zoom := z;
  draw_board b !zoom;
  set_window_title "Boss Battle!";
  get_input p (player_image_gc !zoom) b
