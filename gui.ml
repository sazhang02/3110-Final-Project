open Board

type coords = {
  x : int;
  y : int;
}

type scaling =
  | Small
  | Medium
  | Large

let scale_size = function Small -> 30 | Medium -> 40 | Large -> 50

let get_window_size = function
  | Small -> (780, 480)
  | Medium -> (940, 640)
  | Large -> (1100, 800)

let get_x pos = pos.x

let get_y pos = pos.y

let make_gui_coord x y = { x; y }

let window_size = " 1100x800"

let window_title = "Stuck In The Desert"

let load_png png_name = Png.load_as_rgb24 png_name []

let img_width img = fst (Images.size img)

let img_height img = snd (Images.size img)

let image_name scale file =
  "images/" ^ string_of_int scale ^ "/" ^ file ^ ".png"

let cm_of_image scale_factor name =
  let scale = scale_size scale_factor in
  name |> image_name scale |> load_png

let cm_to_gc scale_factor name =
  name |> cm_of_image scale_factor |> Graphic_image.of_image

let player_image_gc scale = cm_to_gc scale "camel"

let floor_image_gc scale = cm_to_gc scale "floor"

let wall_image_gc scale = cm_to_gc scale "wall"

let coin_image_gc scale = cm_to_gc scale "coin"

let bckg_image_gc scale = cm_to_gc scale "background"

let boss_image_gc scale = cm_to_gc scale "boss"

(* ENTRANCE IMAGES *)

let entr_up_image_gc scale = cm_to_gc scale "entr_up"

let entr_down_image_gc scale = cm_to_gc scale "entr_down"

let entr_left_image_gc scale = cm_to_gc scale "entr_left"

let entr_right_image_gc scale = cm_to_gc scale "entr_right"

(* EXIT IMAGES *)

let exit_up_image_gc scale = cm_to_gc scale "exit_up"

let exit_down_image_gc scale = cm_to_gc scale "exit_down"

let exit_left_image_gc scale = cm_to_gc scale "exit_left"

let exit_right_image_gc scale = cm_to_gc scale "exit_right"

(* GREEN IMAGES *)

let green_up_image_gc scale = cm_to_gc scale "green_up"

let green_down_image_gc scale = cm_to_gc scale "green_down"

let green_left_image_gc scale = cm_to_gc scale "green_left"

let green_right_image_gc scale = cm_to_gc scale "green_right"

(* RED IMAGES *)

let red_up_image_gc scale = cm_to_gc scale "red_up"

let red_down_image_gc scale = cm_to_gc scale "red_down"

let red_left_image_gc scale = cm_to_gc scale "red_left"

let red_right_image_gc scale = cm_to_gc scale "red_right"

(* GOLD IMAGES *)

let gold_up_image_gc scale = cm_to_gc scale "gold_up"

let gold_down_image_gc scale = cm_to_gc scale "gold_down"

let gold_left_image_gc scale = cm_to_gc scale "gold_left"

let gold_right_image_gc scale = cm_to_gc scale "gold_right"

(* BLUE IMAGES *)

let blue_up_image_gc scale = cm_to_gc scale "blue_up"

let blue_down_image_gc scale = cm_to_gc scale "blue_down"

let blue_left_image_gc scale = cm_to_gc scale "blue_left"

let blue_right_image_gc scale = cm_to_gc scale "blue_right"

let tile_width scaling_factor = scale_size scaling_factor

let tile_height scaling_factor = scale_size scaling_factor

let board_to_gui scale_factor (board_coords : Board.coord) =
  {
    x = Board.get_x board_coords * tile_width scale_factor;
    y = Board.get_y board_coords * tile_height scale_factor;
  }

let get_image (loc : coords) zoom =
  Graphics.get_image (get_x loc) (get_y loc) (tile_width zoom)
    (tile_height zoom)

let draw_at_coords img loc = Graphics.draw_image img loc.x loc.y

let update_player new_img old_img new_loc old_loc =
  draw_at_coords old_img old_loc;
  draw_at_coords new_img new_loc

let check_movement old_loc new_loc : bool = old_loc = new_loc

let update_player_boss new_imgs old_imgs new_locs old_locs =
  let old_player_loc = fst old_locs in
  let old_boss_loc = snd old_locs in
  let new_player_loc = fst new_locs in
  let new_boss_loc = snd new_locs in
  let old_player_img = fst old_imgs in
  let new_player_img = fst new_imgs in
  let old_boss_img = snd old_imgs in
  let new_boss_img = snd new_imgs in
  draw_at_coords old_player_img old_player_loc;
  draw_at_coords old_boss_img old_boss_loc;
  draw_at_coords new_player_img new_player_loc;
  draw_at_coords new_boss_img new_boss_loc

type colored_pipes = {
  left : scaling -> Graphics.image;
  right : scaling -> Graphics.image;
  up : scaling -> Graphics.image;
  down : scaling -> Graphics.image;
}

let green_imgs =
  {
    left = green_left_image_gc;
    right = green_right_image_gc;
    up = green_up_image_gc;
    down = green_down_image_gc;
  }

let red_imgs =
  {
    left = red_left_image_gc;
    right = red_right_image_gc;
    up = red_up_image_gc;
    down = red_down_image_gc;
  }

let gold_imgs =
  {
    left = gold_left_image_gc;
    right = gold_right_image_gc;
    up = gold_up_image_gc;
    down = gold_down_image_gc;
  }

let blue_imgs =
  {
    left = blue_left_image_gc;
    right = blue_right_image_gc;
    up = blue_up_image_gc;
    down = blue_down_image_gc;
  }

let choose_colored_img cp t scale =
  match get_tile_orientation t with
  | Left -> cp.left scale
  | Right -> cp.right scale
  | Up -> cp.up scale
  | Down -> cp.down scale

let choose_entr_img t scale =
  match get_tile_orientation t with
  | Left -> entr_left_image_gc scale
  | Right -> entr_right_image_gc scale
  | Up -> entr_up_image_gc scale
  | Down -> entr_down_image_gc scale

let choose_exit_img t scale =
  match get_tile_orientation t with
  | Left -> exit_left_image_gc scale
  | Right -> exit_right_image_gc scale
  | Up -> exit_up_image_gc scale
  | Down -> exit_down_image_gc scale

let choose_pipe_img p t scale =
  match get_pipe_color p with
  | Green -> choose_colored_img green_imgs t scale
  | Red -> choose_colored_img red_imgs t scale
  | Gold -> choose_colored_img gold_imgs t scale
  | Blue -> choose_colored_img blue_imgs t scale
  | Black -> choose_entr_img t scale

let draw_board t scale_factor =
  for i = 0 to get_size t - 1 do
    let tile = get_tile i t in
    let obj_coords =
      Board.get_tile_coords tile |> board_to_gui scale_factor
    in
    match Board.get_tile_type tile with
    | Wall -> draw_at_coords (wall_image_gc scale_factor) obj_coords
    | Pipe p ->
        draw_at_coords (choose_pipe_img p tile scale_factor) obj_coords
    | Entrance _ ->
        draw_at_coords (choose_entr_img tile scale_factor) obj_coords
    | Exit _ ->
        draw_at_coords (choose_exit_img tile scale_factor) obj_coords
    | Empty -> draw_at_coords (floor_image_gc scale_factor) obj_coords
    | Coin -> draw_at_coords (coin_image_gc scale_factor) obj_coords
    (* TODO: draw item not coin*)
    | Item _ -> draw_at_coords (coin_image_gc scale_factor) obj_coords
  done

let display_coins p zoom : unit =
  let loc = get_window_size zoom in
  let x = fst loc - 250 in
  let y = snd loc - 50 in
  Graphics.moveto x y;
  draw_at_coords (bckg_image_gc zoom) (make_gui_coord x y);
  Graphics.draw_string
    ("Coin count: " ^ string_of_int (Player_state.get_coins p))

let display_damage b zoom : unit =
  let loc = get_window_size zoom in
  let x = fst loc - 250 in
  let y = snd loc - 250 in
  Graphics.moveto x y;
  draw_at_coords (bckg_image_gc zoom) (make_gui_coord x y);
  Graphics.draw_string
    ("Health: " ^ string_of_int (Boss_state.get_health b))

let display_steps p zoom : unit =
  let loc = get_window_size zoom in
  let x = fst loc - 250 in
  let y = snd loc - 150 in
  Graphics.moveto x y;
  draw_at_coords (bckg_image_gc zoom) (make_gui_coord x y);
  Graphics.draw_string
    ("Steps: " ^ string_of_int (Player_state.get_steps p))

let unwrap_pb_state zoom resized_player player_prev_img = function
  | p, Some b ->
      let boss_loc =
        Boss_state.get_current_pos b |> board_to_gui zoom
      in
      let boss_prev_img = get_image boss_loc zoom in
      let resized_boss = boss_image_gc zoom in
      Graphics.draw_image resized_boss (get_x boss_loc) (get_y boss_loc);
      ( (resized_player, Some resized_boss),
        (player_prev_img, Some boss_prev_img) )
  | p, None -> ((resized_player, None), (player_prev_img, None))

let redraw_window pb zoom board : unit =
  let window_info = get_window_size zoom in
  let width = fst window_info in
  let height = snd window_info in
  Graphics.resize_window width height;
  draw_board board zoom;
  display_coins (fst pb) zoom

let resize_window_frame
    (pb : Player_state.p * Boss_state.b option)
    zoom
    board :
    (Graphics.image * Graphics.image option)
    * (Graphics.image * Graphics.image option) =
  redraw_window pb zoom board;
  let player_loc =
    Player_state.get_current_pos (fst pb) |> board_to_gui zoom
  in
  let player_prev_img = get_image player_loc zoom in
  let resized_player = player_image_gc zoom in
  Graphics.draw_image resized_player (get_x player_loc)
    (get_y player_loc);
  unwrap_pb_state zoom resized_player player_prev_img pb

let decrease_zoom pb current_imgs prev_imgs zoom board :
    scaling
    * ( (Graphics.image * Graphics.image option)
      * (Graphics.image * Graphics.image option) ) =
  match zoom with
  | Large -> (Medium, resize_window_frame pb Medium board)
  | Medium -> (Small, resize_window_frame pb Small board)
  | Small -> (Small, (current_imgs, prev_imgs))

let increase_zoom pb current_imgs prev_imgs zoom board :
    scaling
    * ( (Graphics.image * Graphics.image option)
      * (Graphics.image * Graphics.image option) ) =
  match zoom with
  | Large -> (Large, (current_imgs, prev_imgs))
  | Medium -> (Large, resize_window_frame pb Large board)
  | Small -> (Medium, resize_window_frame pb Medium board)

(** [starting_loc p zoom] is the coordinates at the beginning of a level
    for the player with state [p]. *)
let starting_loc p zoom =
  Player_state.get_current_pos p |> board_to_gui zoom

let set_up_level p board zoom =
  let loc = starting_loc p zoom in
  draw_board board zoom;
  display_coins p zoom;
  Graphics.draw_image (player_image_gc zoom) (get_x loc) (get_y loc)
