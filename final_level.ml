open Graphics
open Gui
open Player_state

exception Finish_Game

let zoom = ref Large

let empty_tile = Board.make_tile Board.Empty (Board.make_coord 0 0)

let board : Board.t ref =
  ref (Board.alla_board empty_tile empty_tile [] [] [])

(*TODO: Draw boss on new zoomed board*)
let check_item_tile t =
  match Board.get_tile_type t with Board.Item _ -> true | _ -> false

let get_current_imgs t p b board new_locs :
    Graphics.image * Graphics.image =
  (*account for boss?*)
  if check_item_tile t then (
    let player_loc = get_current_pos p in
    let next_item =
      Board.random_item_tile
        (Board.get_x player_loc)
        (Board.get_y player_loc)
        board
    in
    let curr_tile = get_current_tile p in
    Board.set_tile curr_tile board;
    Board.set_tile next_item board;
    draw_at_coords (coin_image_gc !zoom)
      (Board.get_tile_coords next_item |> board_to_gui !zoom);
    (* display_coins p !zoom; *)
    (floor_image_gc !zoom, get_image (snd new_locs) !zoom) )
  else (get_image (fst new_locs) !zoom, get_image (snd new_locs) !zoom)

let new_player_boss_state key player t boss =
  try final_level_update key player t !board boss
  with Levels.UnknownLevel -2 -> raise Finish_Game

let get_loc_info player_boss_state =
  let new_player_loc =
    get_current_pos (fst player_boss_state) |> board_to_gui !zoom
  in
  let new_boss_loc =
    Boss_state.get_current_pos (snd player_boss_state)
    |> board_to_gui !zoom
  in
  (new_player_loc, new_boss_loc)

let rec get_input
    player
    (boss : Boss_state.b)
    (pb_imgs : Graphics.image * Graphics.image)
    prev_imgs
    t : unit =
  match read_key () with
  | 'q' -> close_graph ()
  | 'w' -> move_player 'w' player boss pb_imgs prev_imgs t
  | 'a' -> move_player 'a' player boss pb_imgs prev_imgs t
  | 'd' -> move_player 'd' player boss pb_imgs prev_imgs t
  | 's' -> move_player 's' player boss pb_imgs prev_imgs t
  | 'p' ->
      let resized_info =
        increase_zoom (player, Some boss)
          (fst pb_imgs, Some (snd pb_imgs))
          !zoom !board
      in
      adjust_window resized_info player boss pb_imgs prev_imgs t
  | 'm' ->
      let resized_info =
        decrease_zoom (player, Some boss)
          (fst pb_imgs, Some (snd pb_imgs))
          !zoom !board
      in
      adjust_window resized_info player boss pb_imgs prev_imgs t
  | _ -> get_input player boss pb_imgs prev_imgs t

and adjust_window resized_info p b pb_imgs prev_imgs t : unit =
  let resized_pb = snd resized_info in
  let new_zoom_size = fst resized_info in
  zoom := new_zoom_size;
  match resized_pb with
  | p_img, Some b_img -> get_input p b (p_img, b_img) prev_imgs t
  | _ -> failwith "impossible"

and move_player
    key
    player
    boss
    (pb_imgs : Graphics.image * Graphics.image)
    prev_img
    t : unit =
  let curr_player_boss_loc = get_loc_info (player, boss) in
  let new_pb_state = new_player_boss_state key player t boss in
  let new_player_boss_loc = get_loc_info new_pb_state in
  let tile_in_board =
    Board.get_tile_c (get_current_pos (fst new_pb_state)) !board
  in
  move_player_helper player pb_imgs prev_img new_player_boss_loc
    curr_player_boss_loc new_pb_state tile_in_board t

and move_player_helper
    p
    pb_imgs
    prev_img
    new_pb_loc
    curr_pb_loc
    new_pb_state
    tile
    t =
  let curr_pics =
    get_current_imgs tile (fst new_pb_state) (snd new_pb_state) !board
      new_pb_loc
  in
  check_scenarios p new_pb_state pb_imgs prev_img new_pb_loc curr_pb_loc
    curr_pics t

(* update_player_boss pb_imgs prev_img new_pb_loc curr_pb_loc; get_input
   (fst new_pb_state) (snd new_pb_state) pb_imgs curr_pics t *)
and check_scenarios p new_st pb_imgs prev_img new_pb_loc pb_loc pics t =
  if check_movement (fst pb_loc) (fst new_pb_loc) then begin
    update_player_boss pb_imgs prev_img
      (fst pb_loc, snd new_pb_loc)
      pb_loc;
    get_input p (snd new_st) pb_imgs prev_img t
    (* else if snd new_pb_loc = fst pb_loc then print_endline "f" *)
  end
  else if Boss_state.get_health (snd new_st) = 0 then (
    let boss = Boss_state.set_health (snd new_st) (-100) in
    let exit_pipe_tile = Levels.exit_pipe t (Levels.final_level_id t) in
    Board.set_tile exit_pipe_tile !board;
    draw_board !board !zoom;
    print_endline "EXIT PIPE SET";
    get_input (fst new_st) boss pb_imgs pics t )
  else begin
    update_player_boss pb_imgs prev_img new_pb_loc pb_loc;
    get_input (fst new_st) (snd new_st) pb_imgs pics t
  end

let final_level
    (b : Board.t)
    (z : Gui.scaling)
    (p : Player_state.p)
    (t : Levels.t) =
  print_endline "at final level";
  zoom := z;
  let player_loc = Player_state.get_current_pos p in
  board := b;
  Board.set_tile
    (Board.random_item_tile
       (Board.get_x player_loc)
       (Board.get_y player_loc)
       !board)
    !board;
  set_up_level p b z;
  let entrance_pos =
    Player_state.get_current_pos (Player_state.final_state t !board)
  in
  let boss = Boss_state.init_state entrance_pos !board in
  draw_at_coords (boss_image_gc !zoom)
    (Boss_state.get_current_pos boss |> board_to_gui !zoom);
  set_window_title "Boss Battle!";
  try
    get_input p boss
      (player_image_gc !zoom, boss_image_gc !zoom)
      (floor_image_gc !zoom, floor_image_gc !zoom)
      t
  with Finish_Game -> print_endline "DONE"
