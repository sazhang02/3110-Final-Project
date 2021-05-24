open Graphics
open Gui
open Player_state

type endgame_info = {
  p : Player_state.p;
  p_img : Graphics.image;
  prev_img : Graphics.image;
}

exception Finish_Game of endgame_info

let zoom = ref Large

let steps = ref 0

let empty_tile = Board.make_tile Board.Empty (Board.make_coord 0 0)

let board : Board.t ref =
  ref (Board.alla_board empty_tile empty_tile [] [] [])

let check_item_tile t =
  match Board.get_tile_type t with Board.Item _ -> true | _ -> false

let create_random_item_tile player_loc b board =
  Board.random_item_tile
    (Board.get_x player_loc)
    (Board.get_y player_loc)
    (Boss_state.get_current_pos b)
    board

let display_item b next_item board new_locs =
  if Boss_state.get_health b > 0 then begin
    Board.set_tile next_item board;
    draw_tile !zoom next_item
      (Board.get_tile_coords next_item |> board_to_gui !zoom)
      (Board.get_tile_type next_item);
    (floor_image_gc !zoom, get_image (snd new_locs) !zoom)
  end
  else (floor_image_gc !zoom, get_image (snd new_locs) !zoom)

let create_next_item p b board new_locs =
  let player_loc = get_current_pos p in
  let next_item = create_random_item_tile player_loc b board in
  let curr_tile = get_current_tile p in
  Board.set_tile curr_tile board;
  display_damage b !zoom;
  display_item b next_item board new_locs

let get_current_imgs t p b (board : Board.t) new_locs :
    Graphics.image * Graphics.image =
  if check_item_tile t then create_next_item p b board new_locs
  else (get_image (fst new_locs) !zoom, get_image (snd new_locs) !zoom)

let new_player_boss_state key player t boss p_img prev_img =
  try final_level_update key player t !board boss
  with Levels.UnknownLevel -2 ->
    raise (Finish_Game { p = player; p_img; prev_img })

let get_loc_info player_boss_state =
  let new_player_loc =
    get_current_pos (fst player_boss_state) |> board_to_gui !zoom
  in
  let new_boss_loc =
    Boss_state.get_current_pos (snd player_boss_state)
    |> board_to_gui !zoom
  in
  (new_player_loc, new_boss_loc)

let rec get_input player boss pb_imgs prev_imgs t : unit =
  steps := get_steps player;
  display_damage boss !zoom;
  display_steps player !zoom;
  match read_key () with
  | 'q' -> close_graph ()
  | 'w' -> move_player 'w' player boss pb_imgs prev_imgs t
  | 'a' -> move_player 'a' player boss pb_imgs prev_imgs t
  | 'd' -> move_player 'd' player boss pb_imgs prev_imgs t
  | 's' -> move_player 's' player boss pb_imgs prev_imgs t
  | 'p' -> increase_window_size player boss pb_imgs prev_imgs t
  | 'm' -> decrease_window_size player boss pb_imgs prev_imgs t
  | _ -> get_input player boss pb_imgs prev_imgs t

and adjust_window resized_info p b pb_imgs prev_imgs t : unit =
  let resized_pb = snd resized_info in
  let new_zoom_size = fst resized_info in
  zoom := new_zoom_size;
  match resized_pb with
  | (p_img, Some b_img), (p_prev_img, Some b_prev_img) ->
      get_input p b (p_img, b_img) (p_prev_img, b_prev_img) t
  | _ -> failwith "impossible"

and increase_window_size player boss pb_imgs prev_imgs t =
  let resized_info =
    increase_zoom (player, Some boss)
      (fst pb_imgs, Some (snd pb_imgs))
      (fst prev_imgs, Some (snd prev_imgs))
      !zoom !board
  in
  adjust_window resized_info player boss pb_imgs prev_imgs t

and decrease_window_size player boss pb_imgs prev_imgs t =
  let resized_info =
    decrease_zoom (player, Some boss)
      (fst pb_imgs, Some (snd pb_imgs))
      (fst prev_imgs, Some (snd prev_imgs))
      !zoom !board
  in
  adjust_window resized_info player boss pb_imgs prev_imgs t

and move_player key player boss pb_imgs prev_img t : unit =
  let curr_player_boss_loc = get_loc_info (player, boss) in
  let new_pb_state =
    new_player_boss_state key player t boss (fst pb_imgs) (fst prev_img)
  in
  let new_player_boss_loc = get_loc_info new_pb_state in
  let tile_in_board =
    Board.get_tile_c (get_current_pos (fst new_pb_state)) !board
  in
  move_player_helper player pb_imgs prev_img new_player_boss_loc
    curr_player_boss_loc new_pb_state tile_in_board t

and move_player_helper p pb_imgs prev_img new_loc loc new_pb_st tile t =
  let curr_pics =
    get_current_imgs tile (fst new_pb_st) (snd new_pb_st) !board new_loc
  in
  check_scenarios p new_pb_st pb_imgs prev_img new_loc loc curr_pics t

and player_same_pos p pb_imgs prev_img pb_loc new_pb_loc new_st t =
  update_player_boss pb_imgs prev_img
    (fst pb_loc, snd new_pb_loc)
    pb_loc;
  get_input p (snd new_st) pb_imgs prev_img t

and boss_same_pos pb_imgs prev_img pb_loc new_pb_loc new_st pics t =
  update_player_boss pb_imgs prev_img
    (fst new_pb_loc, snd pb_loc)
    pb_loc;
  get_input (fst new_st) (snd new_st) pb_imgs (fst pics, snd prev_img) t

and player_boss_swap_pos prev_img pb_imgs new_pb_loc pb_loc new_st t =
  let updated_pics =
    if
      check_item_tile
        (Board.get_tile_c (new_st |> fst |> get_current_pos) !board)
    then (snd prev_img, fst prev_img)
    else (floor_image_gc !zoom, fst prev_img)
  in
  update_player_boss pb_imgs prev_img new_pb_loc pb_loc;
  get_input (fst new_st) (snd new_st) pb_imgs updated_pics t

and boss_loc_was_old_player pic prev pb_img new_pb_loc pb_loc new_st t =
  let updated_pics = (fst pic, fst prev) in
  update_player_boss pb_img prev new_pb_loc pb_loc;
  get_input (fst new_st) (snd new_st) pb_img updated_pics t

and player_loc_was_old_boss pic prev pb_img new_pb_loc pb_loc new_st t =
  let updated_pics = (snd prev, snd pic) in
  update_player_boss pb_img prev new_pb_loc pb_loc;
  get_input (fst new_st) (snd new_st) pb_img updated_pics t

and boss_died new_st pb_imgs prev_img =
  raise
    (Finish_Game
       { p = fst new_st; p_img = fst pb_imgs; prev_img = fst prev_img })

and check_scenarios p new_st pb_imgs prev_img new_pb_loc pb_loc pics t =
  if check_movement (fst pb_loc) (fst new_pb_loc) then
    player_same_pos p pb_imgs prev_img pb_loc new_pb_loc new_st t
  else if check_movement (snd pb_loc) (snd new_pb_loc) then
    boss_same_pos pb_imgs prev_img pb_loc new_pb_loc new_st pics t
  else if fst new_pb_loc = snd pb_loc && snd new_pb_loc = fst pb_loc
  then player_boss_swap_pos prev_img pb_imgs new_pb_loc pb_loc new_st t
  else if snd new_pb_loc = fst pb_loc then
    boss_loc_was_old_player pics prev_img pb_imgs new_pb_loc pb_loc
      new_st t
  else if fst new_pb_loc = snd pb_loc then
    player_loc_was_old_boss pics prev_img pb_imgs new_pb_loc pb_loc
      new_st t
  else if Boss_state.get_health (snd new_st) = 0 then
    boss_died new_st pb_imgs prev_img
  else begin
    update_player_boss pb_imgs prev_img new_pb_loc pb_loc;
    get_input (fst new_st) (snd new_st) pb_imgs pics t
  end

let rec quit_game () =
  match read_key () with 'q' -> close_graph () | _ -> quit_game ()

let screen_size_large () =
  let window_info = get_window_size Gui.Large in
  let width = fst window_info in
  let height = snd window_info in
  Graphics.resize_window width height

let endscreen () =
  set_window_title "Game Over!";
  clear_graph ();
  screen_size_large ();
  zoom := Gui.Large;
  draw_screen_background !zoom;
  display_score !steps !zoom;
  try quit_game ()
  with Graphic_failure "fatal I/O error" -> close_graph ()

let rec endgame_input player p_img prev_img t =
  steps := get_steps player;
  display_steps player !zoom;
  match read_key () with
  | 'q' -> close_graph ()
  | 'w' -> move_p 'w' player p_img prev_img t
  | 'a' -> move_p 'a' player p_img prev_img t
  | 'd' -> move_p 'd' player p_img prev_img t
  | 's' -> move_p 's' player p_img prev_img t
  | 'p' -> increase_endgame_window_size player p_img prev_img t
  | 'm' -> decrease_endgame_window_size player p_img prev_img t
  | _ -> endgame_input player p_img prev_img t

and decrease_endgame_window_size player p_img prev_img t =
  let resized_info =
    decrease_zoom (player, None) (p_img, None) (prev_img, None) !zoom
      !board
  in
  adjust_window_endgame resized_info player t

and increase_endgame_window_size player p_img prev_img t =
  let resized_info =
    increase_zoom (player, None) (p_img, None) (prev_img, None) !zoom
      !board
  in
  adjust_window_endgame resized_info player t

and adjust_window_endgame resized_info p t : unit =
  let resized_player = resized_info |> snd |> fst |> fst in
  let resized_prev = resized_info |> snd |> snd |> fst in
  let new_zoom_size = fst resized_info in
  zoom := new_zoom_size;
  endgame_input p resized_player resized_prev t

and move_p key player p_img prev_img t =
  let loc = get_current_pos player |> board_to_gui !zoom in
  let new_player = update key player t !board in
  let new_loc = get_current_pos new_player |> board_to_gui !zoom in
  let curr_pic = get_image new_loc !zoom in
  if check_movement loc new_loc then
    endgame_input new_player p_img prev_img t
  else
    try
      update_player p_img prev_img new_loc loc;
      endgame_input new_player p_img curr_pic t
    with Levels.UnknownLevel -2 -> endscreen ()

let boss_defeated t e =
  let exit_pipe_tile = Levels.exit_pipe t (Levels.final_level_id t) in
  Board.set_tile exit_pipe_tile !board;
  draw_board !board !zoom;
  let p_loc = e.p |> get_current_pos |> board_to_gui !zoom in
  draw_at_coords e.p_img p_loc;
  endgame_input e.p e.p_img e.prev_img t

let setup_references b z p =
  zoom := z;
  board := b;
  steps := Player_state.get_steps p

let setup_initial_board p b z t =
  let player_loc = Player_state.get_current_pos p in
  let entrance_pos =
    Player_state.get_current_pos (Player_state.final_state t !board 0)
  in
  let boss = Boss_state.init_state entrance_pos !board in
  Board.set_tile (create_random_item_tile player_loc boss !board) !board;
  set_up_level p b z;
  draw_at_coords (player_image_gc !zoom)
    (p |> get_current_pos |> board_to_gui !zoom);
  draw_at_coords (boss_image_gc !zoom)
    (Boss_state.get_current_pos boss |> board_to_gui !zoom);
  boss

let final_level b z p t =
  setup_references b z p;
  set_window_title "Boss Battle!";
  let boss = setup_initial_board p b z t in
  try
    get_input p boss
      (player_image_gc !zoom, boss_image_gc !zoom)
      (floor_image_gc !zoom, floor_image_gc !zoom)
      t
  with Finish_Game e -> boss_defeated t e
