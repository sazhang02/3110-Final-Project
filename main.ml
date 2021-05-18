open Graphics
open Gui
open Player_state
open Levels

exception Final_Level

(** [game_of_file] is the game information of all the levels of [file]. *)
let game_of_file file = Yojson.Basic.from_file file |> from_json

(* let game_info = game_of_file "basic_levels.json" *)

(** [level_info] is the game information of all the levels. *)
let game_info = game_of_file "playtest_levels.json"

(** [current_level_id] is the current level the player is on. *)
let current_level_id = ref 0

(** [board_info] is the current board info for the game. *)
let board_info : Board.t array = Levels.make_all_boards game_info

let zoom = ref Large

(** [get_image loc] is the image at [loc]. *)
let get_image (loc : Gui.coords) =
  Graphics.get_image (get_x loc) (get_y loc) (tile_width !zoom)
    (tile_height !zoom)

(** [level_changed p] is True if player has moved onto the next level
    and False otherwise. *)
let level_changed player : bool =
  get_current_level player <> !current_level_id

(** [check_movement old_loc new_loc] is True if [old_loc] is equal to
    [new_loc], otherwise it is False. *)
let check_movement old_loc new_loc : bool = old_loc = new_loc

let check_tile_type t = Board.get_tile_type t = Board.Coin

let new_player_state key player =
  (* if is_final_level game_info !current_level_id then raise
     Final_Level else *)
  try update key player game_info board_info.(!current_level_id) with
  | Levels.UnknownLevel -1 ->
      print_endline "This pipe is locked";
      player
  | Levels.UnknownLevel -2 -> raise Final_Level

let get_current_img t p new_loc : Graphics.image =
  if check_tile_type t then (
    let board = board_info.(!current_level_id) in
    Board.set_tile (get_current_tile p) board;
    display_coins p !zoom;
    floor_image_gc !zoom )
  else get_image new_loc

let rec check_scenarios new_p p_img prev_img new_loc loc curr_pic =
  if level_changed new_p then begin
    current_level_id := get_current_level new_p;
    if is_final_level game_info !current_level_id then raise Final_Level
    else begin
      set_up_level new_p board_info.(!current_level_id) !zoom;
      (* update_player p_img prev_img new_loc loc; *)
      draw_at_coords p_img new_loc;
      get_input new_p p_img curr_pic
    end
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
  let tile_in_board =
    Board.get_tile_c
      (get_current_pos new_player)
      board_info.(!current_level_id)
  in
  let curr_pic = get_current_img tile_in_board new_player new_loc in
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
      let resized_info =
        increase_zoom player player_img !zoom
          board_info.(!current_level_id)
      in
      let resized_player = snd resized_info in
      let new_zoom_size = fst resized_info in
      zoom := new_zoom_size;
      get_input player resized_player prev_image
  | 'm' ->
      let resized_info =
        decrease_zoom player player_img !zoom
          board_info.(!current_level_id)
      in
      let resized_player = snd resized_info in
      let new_zoom_size = fst resized_info in
      zoom := new_zoom_size;
      get_input player resized_player prev_image
  | _ -> get_input player player_img prev_image

(** [window] creates the GUI for the game. *)
let window () =
  open_graph window_size;
  set_window_title window_title;
  (*homescreen here let game_info = something from home screen depending
    on difficulty mode *)
  let player = init_state game_info board_info.(!current_level_id) in
  Gui.set_up_level player board_info.(!current_level_id) !zoom;
  get_input player (player_image_gc !zoom) (floor_image_gc !zoom)

(* Execute the game engine. *)
let () =
  try window () with
  | Graphic_failure "fatal I/O error" -> close_graph ()
  | Final_Level ->
      print_endline "going to final level";
      let last_board_index = Array.length board_info - 1 in
      let last_board = board_info.(last_board_index) in
      let player_final_state = final_state game_info last_board in
      Final_level.final_level last_board !zoom player_final_state
