open Board

type coords = {
  x : int;
  y : int;
}

let get_x pos = pos.x

let get_y pos = pos.y

(* let window_size = " 1000x700" *)
let window_size = " 1100x800"

let window_title = "Stuck In The Desert"

let zoom = ref 0

let load_png png_name = Png.load_as_rgb24 png_name []

let img_width img = fst (Images.size img)

let img_height img = snd (Images.size img)

let scale = 50

let image_name file =
  "images/" ^ string_of_int scale ^ "/" ^ file ^ ".png"

let player_image_cm = "camel" |> image_name |> load_png

let floor_image_cm = "floor" |> image_name |> load_png

let wall_image_cm = "wall" |> image_name |> load_png

let pipeup_image_cm = "pipe_up" |> image_name |> load_png

let pipedown_image_cm = "pipe_down" |> image_name |> load_png

let pipeleft_image_cm = "pipe_left" |> image_name |> load_png

let piperight_image_cm = "pipe_right" |> image_name |> load_png

let player2 = load_png "images/player2.png"

let player3 = load_png "images/player3.png"

let player_image_gc () = Graphic_image.of_image player_image_cm

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

let entrance_image_gc () = Graphic_image.of_image player2

let exit_image_gc () = Graphic_image.of_image player3

let tile_width = fst (Images.size player_image_cm)

let tile_height = snd (Images.size player_image_cm)

let board_to_gui (board_coords : Board.coord) =
  { x = board_coords.x * tile_width; y = board_coords.y * tile_height }

let draw_at_coords img loc = Graphics.draw_image img loc.x loc.y

let update_player new_img old_img new_loc old_loc =
  draw_at_coords old_img old_loc;
  draw_at_coords new_img new_loc

let choose_pipe_img p =
  match get_pipe_orientation p with
  | Left -> pipeleft_image_gc ()
  | Right -> piperight_image_gc ()
  | Up -> pipeup_image_gc ()
  | Down -> pipedown_image_gc ()

let draw_board t =
  for i = 0 to get_size t - 1 do
    let tile = get_tile i t in
    let obj_coords = tile.coords |> board_to_gui in
    match tile.tile_type with
    | Wall -> draw_at_coords (wall_image_gc ()) obj_coords
    | Pipe p -> draw_at_coords (choose_pipe_img p) obj_coords
    | Entrance -> draw_at_coords (entrance_image_gc ()) obj_coords
    | Exit -> draw_at_coords (exit_image_gc ()) obj_coords
    | Empty -> draw_at_coords (floor_image_gc ()) obj_coords
  done
