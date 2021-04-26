open Graphics
open Gui
open Player_state
open Levels

(** [game_of_file] is the game information of all the levels of [file]. *)
let game_of_file file = Yojson.Basic.from_file file |> from_json

(** [level_info] is the game information of all the levels. *)
let game_info = game_of_file "basic_levels.json"

(** [current_level_id] is the current level the player is on. *)
let current_level_id = ref 0

let zoom = ref Large

let resize_window_frame player : Graphics.image =
  let window_info = get_window_size !zoom in
  let width = fst window_info in
  let height = snd window_info in
  resize_window width height;
  draw_board (make_board game_info !current_level_id) !zoom;
  let loc = get_current_pos player |> board_to_gui !zoom in
  let resized_player = player_image_gc !zoom in
  draw_image resized_player (get_x loc) (get_y loc);
  resized_player

let decrease_zoom player current_image : Graphics.image =
  match !zoom with
  | Large ->
      zoom := Medium;
      resize_window_frame player
  | Medium ->
      zoom := Small;
      resize_window_frame player
  | Small -> current_image

(**)
let increase_zoom player current_image : Graphics.image =
  match !zoom with
  | Large -> current_image
  | Medium ->
      zoom := Large;
      resize_window_frame player
  | Small ->
      zoom := Medium;
      resize_window_frame player

(** [board_info] is the current board info for the game. *)
let board_info =
  Array.make
    (game_info |> Levels.get_levels |> List.length)
    (make_board game_info !current_level_id)

(** [player] is the initial state the of the player. *)
let player = init_state game_info board_info.(!current_level_id)

(** [starting_loc p] is the coordinates at the beginning of a level for
    the player wi th state [p]. *)
let starting_loc p = get_current_pos p |> board_to_gui !zoom

(** [get_image loc] is the image at [loc]. *)
let get_image (loc : Gui.coords) =
  Graphics.get_image (get_x loc) (get_y loc) (tile_width !zoom)
    (tile_height !zoom)

(** [set_up_level level_id loc] sets up the level corresponding to
    [level_id]. The board for the level is drawn, and the player is
    drawn at the coordinates [loc]. *)
let set_up_level level_id player =
  (* print_endline (string_of_int level_id); *)
  let loc = starting_loc player in
  board_info.(level_id) <- make_board game_info level_id;
  draw_board (make_board game_info level_id) !zoom;
  draw_image (player_image_gc !zoom) (get_x loc) (get_y loc)

(** [level_changed p] is True if player has moved onto the next level
    and False otherwise. *)
let level_changed p : bool = get_current_level p <> !current_level_id

(** [check_movement old_loc new_loc] is True if [old_loc] is equal to
    [new_loc], otherwise it is False. *)
let check_movement old_loc new_loc : bool = old_loc = new_loc

(** [get_input loc player_img prev_image] processes keyboard inputs
    where w a s d moves the player up left down right respectively.
    Pressing q quits the game and anything else re-prompts the user for
    inputs. *)
let rec get_input player player_img prev_image =
  let move_player key =
    let loc = get_current_pos player |> board_to_gui !zoom in
    let new_player_state =
      try
        update key player game_info board_info.(!current_level_id)
      with
      | Levels.UnknownLevel -1 ->
          print_endline "This pipe is locked";
          player
      | Levels.UnknownLevel -2 ->
          print_endline "BOSS BATTLEEEE";
          player
    in
    let new_loc =
      get_current_pos new_player_state |> board_to_gui !zoom
    in
    let current_pic = get_image new_loc in
    (* Check if player has reached an exit pipe *)
    if level_changed new_player_state then begin
      current_level_id := get_current_level new_player_state;
      set_up_level !current_level_id new_player_state;
      update_player player_img prev_image new_loc loc;
      get_input new_player_state player_img current_pic
    end
    else if check_movement loc new_loc then
      get_input new_player_state player_img prev_image
    else begin
      update_player player_img prev_image new_loc loc;
      get_input new_player_state player_img current_pic
    end
  in
  match read_key () with
  | 'w' -> move_player 'w'
  | 's' -> move_player 's'
  | 'a' -> move_player 'a'
  | 'd' -> move_player 'd'
  | 'q' -> close_graph ()
  | 'p' ->
      let resized_player = increase_zoom player player_img in
      get_input player resized_player prev_image
  | 'm' ->
      let resized_player = decrease_zoom player player_img in
      get_input player resized_player prev_image
  | _ -> get_input player player_img prev_image

(** [window] creates the GUI for the game. *)
let window () =
  open_graph window_size;
  set_window_title window_title;
  set_up_level !current_level_id player;
  get_input player (player_image_gc !zoom) (floor_image_gc !zoom)

(* Execute the game engine. *)
let () =
  try window ()
  with Graphic_failure "fatal I/O error" -> close_graph ()
