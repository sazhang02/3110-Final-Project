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
let board_info : Board.t array = Levels.make_all_boards game_info

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
  let loc = starting_loc player in
  draw_board board_info.(level_id) !zoom;
  draw_image (player_image_gc !zoom) (get_x loc) (get_y loc)

(** [level_changed p] is True if player has moved onto the next level
    and False otherwise. *)
let level_changed player : bool =
  get_current_level player <> !current_level_id

(** [check_movement old_loc new_loc] is True if [old_loc] is equal to
    [new_loc], otherwise it is False. *)
let check_movement old_loc new_loc : bool = old_loc = new_loc

let check_tile_type t = Board.get_tile_type t = Board.Coin

let new_player_state key player =
  try update key player game_info board_info.(!current_level_id) with
  | Levels.UnknownLevel -1 ->
      print_endline "This pipe is locked";
      player
  | Levels.UnknownLevel -2 ->
      print_endline "BOSS BATTLEEEE";
      player

let replace_coin_tile t p =
  if check_tile_type t then
    let board = board_info.(!current_level_id) in
    Board.set_tile (get_current_tile p) board
  else ()

let rec check_scenarios new_p p_img prev_img new_loc loc curr_pic =
  if level_changed new_p then begin
    current_level_id := get_current_level new_p;
    set_up_level !current_level_id new_p;
    update_player p_img prev_img new_loc loc;
    get_input new_p p_img curr_pic
  end
  else if check_movement loc new_loc then get_input new_p p_img prev_img
  else begin
    update_player p_img prev_img new_loc loc;
    get_input new_p p_img curr_pic
  end

and move_player key player player_img prev_image =
  let loc = get_current_pos player |> board_to_gui !zoom in
  let new_player = new_player_state key player in
  let new_loc = get_current_pos new_player |> board_to_gui !zoom in
  let curr_pic = get_image new_loc in
  let tile_in_board =
    Board.get_tile_c
      (get_current_pos new_player)
      board_info.(!current_level_id)
  in
  replace_coin_tile tile_in_board new_player;
  check_scenarios new_player player_img prev_image new_loc loc curr_pic

(** [get_input loc player_img prev_image] processes keyboard inputs
    where w a s d moves the player up left down right respectively.
    Pressing q quits the game and anything else re-prompts the user for
    inputs. *)
and get_input player player_img prev_image : unit =
  match read_key () with
  | 'w' -> move_player 'w' player player_img prev_image
  | 's' -> move_player 's' player player_img prev_image
  | 'a' -> move_player 'a' player player_img prev_image
  | 'd' -> move_player 'd' player player_img prev_image
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
  let player = init_state game_info board_info.(!current_level_id) in
  set_up_level !current_level_id player;
  get_input player (player_image_gc !zoom) (floor_image_gc !zoom)

(* Execute the game engine. *)
let () =
  try window ()
  with Graphic_failure "fatal I/O error" -> close_graph ()
