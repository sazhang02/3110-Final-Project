open Graphics
open Gui
open Player_state
open Levels

(** [level_info] is the game information of all the levels. *)
let level_info = Yojson.Basic.from_file "basic_levels.json" |> from_json

(** [current_level_id] is the current level the player is on. *)
let current_level_id = ref 0

(** [player] is the initial state the of the player. *)
let player = init_state level_info !current_level_id

(** [starting_loc p] is the coordinates at the beginning of a level for
    the player wi th state [p]. *)
let starting_loc p = get_current_pos p |> board_to_gui

(** [get_image loc] is the image at [loc]. *)
let get_image (loc : Gui.coords) =
  Graphics.get_image (get_x loc) (get_y loc) tile_width tile_height

(** [set_up_level level_id p loc] sets up the level corresponding to
    [level_id]. The board for the level is drawn, and the player is
    drawn at the coordinates [loc]. *)
let set_up_level level_id loc =
  draw_board (make_board level_info !current_level_id);
  draw_image (player_image_gc ()) (get_x loc) (get_y loc)

(** [level_changed p] is True if player has moved onto the next level
    and False otherwise. *)
let level_changed player : bool =
  get_current_level p <> !current_level_id

(** [get_input loc player_img prev_image] processes keyboard inputs
    where w a s d moves the player up left down right respectively.
    Pressing q quits the game and anything else re-prompts the user for
    inputs. *)
let rec get_input player player_img prev_image =
  let move_player key =
    let loc = get_current_pos player in
    let new_player_state = update key player level_info in
    let new_loc = get_current_pos new_player_state |> board_to_gui in
    let current_pic = get_image new_loc in
    (*Check if player has reached an exit pipe*)
    if level_changed new_player_state then begin
      current_level_id := get_current_level new_player_state;
      set_up_level current_level_id new_loc;
      update_player player_img prev_image new_loc (loc |> board_to_gui);
      get_input new_player_state player_img current_pic
    end
    else begin
      update_player player_img prev_image new_loc (loc |> board_to_gui);
      get_input new_player_state player_img current_pic
    end
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
  let loc = starting_loc player in
  let starting_image = get_image loc in
  set_up_level current_level_id loc;
  get_input player (player_image_gc ()) starting_image

(* Execute the game engine. *)
let () =
  try window ()
  with Graphic_failure "fatal I/O error" -> close_graph ()
