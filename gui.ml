open Board

type coords = {
  x : int;
  y : int;
}

type scaling =
  | Small
  | Medium
  | Large

let scale_size = function Small -> 20 | Medium -> 30 | Large -> 50

let get_window_size = function
  | Small -> (500, 500)
  | Medium -> (780, 480)
  | Large -> (1100, 800)

let get_x pos = pos.x

let get_y pos = pos.y

(* let window_size = " 1000x700" *)
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

let player_image_cm scale_factor = cm_of_image scale_factor "camel"

let floor_image_cm scale_factor = cm_of_image scale_factor "floor"

let wall_image_cm scale_factor = cm_of_image scale_factor "wall"

let pipeup_image_cm scale_factor = cm_of_image scale_factor "pipe_up"

let pipedown_image_cm scale_factor =
  cm_of_image scale_factor "pipe_down"

let pipeleft_image_cm scale_factor =
  cm_of_image scale_factor "pipe_left"

let piperight_image_cm scale_factor =
  cm_of_image scale_factor "pipe_right"

let entr_up_image_cm scale_factor = cm_of_image scale_factor "entr_up"

let entr_down_image_cm scale_factor =
  cm_of_image scale_factor "entr_down"

let entr_left_image_cm scale_factor =
  cm_of_image scale_factor "entr_left"

let entr_right_image_cm scale_factor =
  cm_of_image scale_factor "entr_right"

let player2 = load_png "images/player2.png"

let player3 = load_png "images/player3.png"

let player_image_gc scale =
  Graphic_image.of_image (player_image_cm scale)

let floor_image_gc scale = Graphic_image.of_image (floor_image_cm scale)

(** [wall_image_gc ()] is [wall_image_cm] as a Graphics.image. *)
let wall_image_gc scale = Graphic_image.of_image (wall_image_cm scale)

(** [pipeup_image_gc ()] is [pipeup_image_cm] as a Graphics.image. *)
let pipeup_image_gc scale =
  Graphic_image.of_image (pipeup_image_cm scale)

(** [pipedown_image_gc ()] is [pipedown_image_cm] as a Graphics.image. *)
let pipedown_image_gc scale =
  Graphic_image.of_image (pipedown_image_cm scale)

(** [pipeleft_image_gc scale] is [pipeleft_image_cm] as a
    Graphics.image. *)
let pipeleft_image_gc scale =
  Graphic_image.of_image (pipeleft_image_cm scale)

(** [piperight_image_gc ()] is [piperight_image_cm] as a Graphics.image. *)
let piperight_image_gc scale =
  Graphic_image.of_image (piperight_image_cm scale)

let entrance_image_gc () = Graphic_image.of_image player2

let exit_image_gc () = Graphic_image.of_image player3

let tile_width scaling_factor =
  fst (Images.size (scaling_factor |> player_image_cm))

let tile_height scaling_factor =
  snd (Images.size (scaling_factor |> player_image_cm))

let board_to_gui scale_factor (board_coords : Board.coord) =
  (* let scale = scale_size scale_factor in *)
  {
    x = Board.get_x board_coords * tile_width scale_factor;
    y = Board.get_y board_coords * tile_height scale_factor;
  }

(* { x = board_coords.x * tile_width; y = board_coords.y * tile_height } *)

let draw_at_coords img loc = Graphics.draw_image img loc.x loc.y

let update_player new_img old_img new_loc old_loc =
  draw_at_coords old_img old_loc;
  draw_at_coords new_img new_loc

let choose_pipe_img t scale =
  match get_tile_orientation t with
  | Left -> pipeleft_image_gc scale
  | Right -> piperight_image_gc scale
  | Up -> pipeup_image_gc scale
  | Down -> pipedown_image_gc scale

(* let scale_gui = function | Small -> | Medium -> | Large -> | _ *)

let draw_board t scale_factor =
  for i = 0 to get_size t - 1 do
    let tile = get_tile i t in
    let obj_coords =
      Board.get_tile_coords tile |> board_to_gui scale_factor
    in
    (* let scale = scale_size scale_factor in *)
    match Board.get_tile_type tile with
    | Wall -> draw_at_coords (wall_image_gc scale_factor) obj_coords
    | Pipe _ ->
        draw_at_coords (choose_pipe_img tile scale_factor) obj_coords
    | Entrance _ -> draw_at_coords (entrance_image_gc ()) obj_coords
    | Exit _ -> draw_at_coords (exit_image_gc ()) obj_coords
    | Empty -> draw_at_coords (floor_image_gc scale_factor) obj_coords
  done
