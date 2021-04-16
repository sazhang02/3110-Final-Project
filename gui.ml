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

let wall_image_cm = load_png "images/wall tile.png"

let cactus_image_cm = load_png "images/cactus tile.png"

(** [player_image_gc ()] is the Images.t of a player's image as a
    Graphics.image. *)
let player_image_gc () = Graphic_image.of_image player_image_cm

let floor_image_gc () = Graphic_image.of_image floor_image_cm

let wall_image_gc () = Graphic_image.of_image wall_image_cm

let cactus_image_gc () = Graphic_image.of_image cactus_image_cm

let tile_width = fst (Images.size player_image_cm)

let tile_height = snd (Images.size player_image_cm)

let draw_at_coords img loc = Graphics.draw_image img loc.x loc.y

let update_player new_img old_img new_loc old_loc =
  Graphics.draw_image old_img old_loc.x old_loc.y;
  draw_at_coords new_img new_loc

(* TODO: replace with correct images *)
let draw_board t =
  (* let board_array = get_tile_array t in *)
  for i = 0 to Array.length t - 1 do
    let obj_coords = t.(i).coords |> board_to_gui in
    match t.(i).tile_type with
    | Wall _ -> draw_at_coords (floor_image_gc ()) obj_coords
    | Pipe _ -> draw_at_coords (cactus_image_gc ()) obj_coords
    | Entrance -> ()
    | Exit -> ()
    | Empty -> draw_at_coords (floor_image_gc ()) obj_coords
  done
