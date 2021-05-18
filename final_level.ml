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

(* and move_player key player player_img prev_image b = let loc =
   get_current_pos player |> board_to_gui !zoom in let new_player =
   new_player_state key player in let new_loc = get_current_pos
   new_player |> board_to_gui !zoom in let tile_in_board =
   Board.get_tile_c (get_current_pos new_player)
   board_info.(!current_level_id) in let curr_pic = get_current_img
   tile_in_board new_player new_loc in failwith "" *)

let final_level (b : Board.t) (z : Gui.scaling) (p : Player_state.p) =
  print_endline "at final level";
  zoom := z;
  set_up_level p b z;
  let boss = Boss_state.init_state b in
  draw_at_coords (boss_image_gc !zoom)
    (Boss_state.get_current_pos boss |> board_to_gui !zoom);
  set_window_title "Boss Battle!";
  get_input p (player_image_gc !zoom) b
