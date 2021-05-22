open Graphics
open Gui
open Player_state
open Levels

(* Raised to indicate that the player has moved on to the final level. *)
exception Final_Level

(** [game_of_file] is the game information of all the levels of [file]. *)
let game_of_file file = Yojson.Basic.from_file file |> from_json

(** [setup_game] will call Homescreen and setup the game immediately. *)
let setup_game =
  open_graph window_size;
  set_window_title window_title;
  Homescreen.homescreen ()

(** [game_info] is the game information of all the levels. *)
let game_info = game_of_file setup_game

(** [current_level_id] is the current level the player is on. *)
let current_level_id = ref 0

(** [board_info] is the current board info for all the boards in the
    game. *)
let board_info : Board.t array = Levels.make_all_boards game_info

(** [zoom] is the current zoom size. *)
let zoom = ref Large

(** [steps] is the current steps the player has taken. *)
let steps = ref 0

(** [level_changed p] is checks if the player has moved onto the next
    level. *)
let level_changed player : bool =
  get_current_level player <> !current_level_id

(** [check_tile_type t] is checks if the type of tile [t] is a coin. *)
let check_tile_type t = Board.get_tile_type t = Board.Coin

(** [new_player_state key player] is the new Player_state.p after [key]
    is inputted. Prints to the terminal a message if the player tries to
    enter the first entrance pipe of the game (as there is nothing to go
    back to). *)
let new_player_state key player =
  steps := get_steps player;
  try update key player game_info board_info.(!current_level_id)
  with Levels.UnknownLevel -1 ->
    print_endline "This pipe is locked";
    player

(** [get_current_img t p new_loc] is the Graphics.image at of where the
    player is currently located. This will also update the display if
    the player just collected a coin.*)
let get_current_img t p new_loc : Graphics.image =
  if check_tile_type t then (
    let board = board_info.(!current_level_id) in
    Board.set_tile (get_current_tile p) board;
    display_coins p !zoom;
    floor_image_gc !zoom )
  else get_image new_loc !zoom

(* Main logic for the game. *)

(** [get_input loc player_img prev_image] processes keyboard inputs
    where 'w', 'a', 's', 'd' moves the player up, left, down, right,
    respectively. Pressing 'q' quits the game and anything else
    re-prompts the user for inputs. *)
let rec get_input player player_img prev_image : unit =
  display_steps player !zoom;
  match read_key () with
  | 'w' -> move_player 'w' player player_img prev_image
  | 's' -> move_player 's' player player_img prev_image
  | 'a' -> move_player 'a' player player_img prev_image
  | 'd' -> move_player 'd' player player_img prev_image
  | 'q' -> close_graph ()
  | 'p' -> increase_window_size player player_img prev_image
  | 'm' -> decrease_window_size player player_img prev_image
  | _ -> get_input player player_img prev_image

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

and check_scenarios new_p p_img prev_img new_loc loc curr_pic =
  if level_changed new_p then
    player_goes_next_level new_p p_img new_loc curr_pic
  else if check_movement loc new_loc then get_input new_p p_img prev_img
  else begin
    update_player p_img prev_img new_loc loc;
    get_input new_p p_img curr_pic
  end

and player_goes_next_level new_p p_img new_loc curr_pic =
  current_level_id := get_current_level new_p;
  if is_final_level game_info !current_level_id then raise Final_Level
  else begin
    set_up_level new_p board_info.(!current_level_id) !zoom;
    draw_at_coords p_img new_loc;
    get_input new_p p_img curr_pic
  end

and increase_window_size player player_img prev_image =
  let resized_info =
    increase_zoom (player, None) (player_img, None) (prev_image, None)
      !zoom
      board_info.(!current_level_id)
  in
  adjust_window resized_info player

and decrease_window_size player player_img prev_image =
  let resized_info =
    decrease_zoom (player, None) (player_img, None) (prev_image, None)
      !zoom
      board_info.(!current_level_id)
  in
  adjust_window resized_info player

and adjust_window resized_info p : unit =
  let resized_player = resized_info |> snd |> fst |> fst in
  let resized_prev = resized_info |> snd |> snd |> fst in
  let new_zoom_size = fst resized_info in
  zoom := new_zoom_size;
  get_input p resized_player resized_prev

(** [window] starts the GUI for the game. *)
let window () =
  clear_graph ();
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
      let player_final_state =
        final_state game_info last_board !steps
      in
      Final_level.final_level last_board !zoom player_final_state
        game_info
