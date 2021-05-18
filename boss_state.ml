open Board

type b = {
  current_tile : tile;
  health : int;
}

let init_state bt =
  (* let coord = make_coord (Random.int 15) (Random.int 15) in *)
  let coord = make_coord 0 0 in
  { current_tile = get_tile_c coord bt; health = 100 }

let get_current_tile (b : b) = b.current_tile

let get_current_pos (b : b) = get_tile_coords b.current_tile

let get_health (b : b) = b.health

let decrease_health b : b = { b with health = b.health - 1 }

let get_distance p_pos b_pos : float =
  let player_x = get_x p_pos |> float_of_int in
  let player_y = get_y p_pos |> float_of_int in
  let boss_x = get_x b_pos |> float_of_int in
  let boss_y = get_y b_pos |> float_of_int in
  sqrt (((player_x -. boss_x) ** 2.) +. ((player_y -. boss_y) ** 2.))

let get_min distances init =
  let rec get_min_helper lst min =
    match lst with
    | [] -> min
    | (distance, pos) :: t -> get_min_helper t (Stdlib.min distance min)
  in
  let min = get_min_helper distances init in
  List.assoc min distances

let get_closest_pos p_pos b_pos : Board.coord =
  let b_pos_up = make_coord (get_x b_pos) (get_y b_pos + 1) in
  let up_distance = get_distance p_pos b_pos_up in
  let b_pos_down = make_coord (get_x b_pos) (get_y b_pos - 1) in
  let down_distance = get_distance p_pos b_pos_down in
  let b_pos_left = make_coord (get_x b_pos - 1) (get_y b_pos) in
  let left_distance = get_distance p_pos b_pos_left in
  let b_pos_right = make_coord (get_x b_pos + 1) (get_y b_pos) in
  let right_distance = get_distance p_pos b_pos_right in
  let distances =
    [
      (up_distance, b_pos_up);
      (down_distance, b_pos_down);
      (left_distance, b_pos_left);
      (right_distance, b_pos_right);
    ]
  in
  get_min distances up_distance

let move_boss (p_pos : Board.coord) b board : b =
  let boss_pos = get_current_pos b in
  let new_boss_pos = get_closest_pos p_pos boss_pos in
  { b with current_tile = get_tile_c new_boss_pos board }
