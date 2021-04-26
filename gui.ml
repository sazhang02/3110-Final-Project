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

let draw_at_coords img loc = Graphics.draw_image img loc.x loc.y

let update_player new_img old_img new_loc old_loc =
  draw_at_coords old_img old_loc;
  draw_at_coords new_img new_loc

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
    | Coin exists ->
        if exists then failwith "add coin pic"
        else draw_at_coords (floor_image_gc scale_factor) obj_coords
  done
