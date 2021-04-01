open Graphics
open Gui
open Player_state

(* let player_image_cm = Gui.load_png "images/player.png"

   (** [player_image_gc ()] is the Images.t of a player's image as a
   Graphics.image. *) let player_image_gc () = Graphic_image.of_image
   player_image_cm

   let tile_width = fst (Images.size player_image_cm)

   let tile_height = snd (Images.size player_image_cm) *)

let starting_loc = { x = 500; y = 500 }

(** [get_image loc] is the image at [loc]. *)
let get_image loc =
  Graphics.get_image loc.x loc.y tile_width tile_height

(** [get_input loc player_img prev_image] processes keyboard inputs
    where w a s d moves the player up left down right respectively.
    Pressing q quits the game and anything else re-prompts the user for
    inputs. *)
let rec get_input loc player_img prev_image =
  match read_key () with
  | 'w' ->
      let new_loc = { loc with y = loc.y + tile_height } in
      let current_pic = get_image new_loc in
      update_player player_img prev_image new_loc loc;
      get_input new_loc player_img current_pic
  | 's' ->
      let new_loc = { loc with y = loc.y - tile_height } in
      let current_pic = get_image new_loc in
      update_player player_img prev_image new_loc loc;
      get_input new_loc player_img current_pic
  | 'a' ->
      let new_loc = { loc with x = loc.x - tile_width } in
      let current_pic = get_image new_loc in
      update_player player_img prev_image new_loc loc;
      get_input new_loc player_img current_pic
  | 'd' ->
      let new_loc = { loc with x = loc.x + tile_width } in
      let current_pic = get_image new_loc in
      update_player player_img prev_image new_loc loc;
      get_input new_loc player_img current_pic
  | 'q' -> close_graph ()
  | _ -> get_input loc player_img prev_image

(** [window] creates the GUI for the game. *)
let window () =
  open_graph window_size;
  set_window_title window_title;
  let starting_image = get_image starting_loc in
  draw_image (floor_image_gc ()) (starting_loc.x - 200)
    (starting_loc.y - 200);
  draw_image (player_image_gc ()) starting_loc.x starting_loc.y;
  get_input starting_loc (player_image_gc ()) starting_image

(* Execute the game engine. *)
let () =
  try window ()
  with Graphic_failure "fatal I/O error" -> close_graph ()
