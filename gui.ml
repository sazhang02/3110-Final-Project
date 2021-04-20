open Board

type coords = {
  x : int;
  y : int;
}

let get_x pos = pos.x

let get_y pos = pos.y

let window_size = " 1000x700"

let window_title = "Stuck In The Desert"

let load_png png_name = Png.load_as_rgb24 png_name []

let img_width img = fst (Images.size img)

let img_height img = snd (Images.size img)

let board_to_gui (board_coords : Board.coord) =
  { x = board_coords.x * 50; y = board_coords.y * 50 }

let player_image_cm = load_png "images/player.png"

let floor_image_cm = load_png "images/Floor Tile.png"

let wall_image_cm = load_png "images/wall.png"

let pipeup_image_cm = load_png "images/pipe_up.png"

let pipedown_image_cm = load_png "images/pipe_down.png"

let pipeleft_image_cm = load_png "images/pipe_left.png"

let piperight_image_cm = load_png "images/pipe_right.png"

(** [player_image_gc ()] is [player_image_cm] as a Graphics.image. *)
let player_image_gc () = Graphic_image.of_image player_image_cm

(** [floor_image_gc ()] is [floor_image_cm] as a Graphics.image. *)
let floor_image_gc () = Graphic_image.of_image floor_image_cm

(** [wall_image_gc ()] is [wall_image_cm] as a Graphics.image. *)
let wall_image_gc () = Graphic_image.of_image wall_image_cm

(** [pipeup_image_gc ()] is [pipeup_image_cm] as a Graphics.image. *)
let pipeup_image_gc () = Graphic_image.of_image pipeup_image_cm

(** [pipedown_image_gc ()] is [pipedown_image_cm] as a Graphics.image. *)
let pipedown_image_gc () = Graphic_image.of_image pipedown_image_cm

(** [pipeleft_image_gc ()] is [pipeleft_image_cm] as a Graphics.image. *)
let pipeleft_image_gc () = Graphic_image.of_image pipeleft_image_cm

(** [piperight_image_gc ()] is [piperight_image_cm] as a Graphics.image. *)
let piperight_image_gc () = Graphic_image.of_image piperight_image_cm

let tile_width = fst (Images.size player_image_cm)

let tile_height = snd (Images.size player_image_cm)

let draw_at_coords img loc = Graphics.draw_image img loc.x loc.y

let update_player new_img old_img new_loc old_loc =
  draw_at_coords old_img old_loc;
  draw_at_coords new_img new_loc

let draw_board t =
  (* let board_array = get_tile_array t in *)
  for i = 0 to get_size t - 1 do
    let tile = get_tile i t in
    let obj_coords = tile.coords |> board_to_gui in
    match tile.tile_type with
    | Wall -> draw_at_coords (wall_image_gc ()) obj_coords
    | Pipe _ -> draw_at_coords (pipeup_image_gc ()) obj_coords
    | Entrance -> ()
    | Exit -> ()
    | Empty -> draw_at_coords (floor_image_gc ()) obj_coords
  done
