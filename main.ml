open Graphics
open Gui
open Player_state
open Levels

(* let player_image_cm = Gui.load_png "images/player.png"

   (** [player_image_gc ()] is the Images.t of a player's image as a
   Graphics.image. *) let player_image_gc () = Graphic_image.of_image
   player_image_cm

   let tile_width = fst (Images.size player_image_cm)

   let tile_height = snd (Images.size player_image_cm) *)
let level_info = Yojson.Basic.from_file "basic_levels.json" |> from_json

let current_level_id = 0

let player = init_state level_info current_level_id

let starting_loc = get_current_pos player

(** [get_image loc] is the image at [loc]. *)
let get_image (loc : Gui.coords) =
  Graphics.get_image (get_x loc) (get_y loc) tile_width tile_height

(** [get_input loc player_img prev_image] processes keyboard inputs
    where w a s d moves the player up left down right respectively.
    Pressing q quits the game and anything else re-prompts the user for
    inputs. *)
let rec get_input player player_img prev_image =
  let move_player key =
    let loc = get_current_pos player in
    let new_player_state = update key player in
    let new_loc = get_current_pos new_player_state in
    let current_pic = get_image new_loc in
    update_player player_img prev_image new_loc loc;
    get_input new_player_state player_img current_pic
  in
  match read_key () with
  | 'w' -> move_player 'w'
  | 's' -> move_player 's'
  | 'a' -> move_player 'a'
  | 'd' -> move_player 'd'
  | 'q' -> close_graph ()
  | _ -> get_input player player_img prev_image

(** [window] creates the GUI for the game. *)

let window () =
  open_graph window_size;
  set_window_title window_title;
  draw_board (make_board level_info current_level_id);
  let starting_image = get_image starting_loc in
  draw_image (floor_image_gc ())
    (get_x starting_loc - 200)
    (get_y starting_loc - 200);
  draw_image (player_image_gc ()) (get_x starting_loc)
    (get_y starting_loc);
  get_input player (player_image_gc ()) starting_image

(* Execute the game engine. *)
let () =
  try window ()
  with Graphic_failure "fatal I/O error" -> close_graph ()

(* let main () = try window () with Graphic_failure "fatal I/O error" ->
   close_graph () get_input player (player_image_gc ()) starting_image
   (* Execute the game engine. *) let () = main () (* try window () with
   Graphic_failure "fatal I/O error" -> close_graph () *) *)
