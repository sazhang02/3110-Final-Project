open Board

let coords_to_string c =
  "(" ^ string_of_int (get_x c) ^ ", " ^ string_of_int (get_y c) ^ ")"

type b = {
  current_tile : tile;
  health : int;
}

let rec init_state entrance_pos bt =
  Random.self_init ();
  let x = random_int 0 (get_x entrance_pos) 16 3 in
  let y = random_int 0 (get_y entrance_pos) 16 3 in
  let coord = make_coord x y in
  let tile = get_tile_c coord bt in
  match get_tile_type tile with
  | Empty -> { current_tile = tile; health = 100 }
  | _ -> init_state entrance_pos bt

let get_current_tile (b : b) = b.current_tile

let get_current_pos (b : b) = get_tile_coords b.current_tile

let get_health (b : b) = b.health

let set_health (b : b) h : b = { b with health = h }

let decrease_health b amount =
  if b.health - amount > 0 then { b with health = b.health - amount }
  else { b with health = 0 }

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

  (* let distances = [ (up_distance, b_pos_up); (down_distance,
     b_pos_down); (left_distance, b_pos_left); (right_distance,
     b_pos_right); ] in get_min distances up_distance *)
  b_pos

let move_bosss (p_pos : Board.coord) b board : b =
  let boss_pos = get_current_pos b in
  let new_boss_pos = get_closest_pos p_pos boss_pos in
  { b with current_tile = get_tile_c new_boss_pos board }

let move_boss (p_pos : Board.coord) b board : b =
  let boss_pos = get_current_pos b in
  let new_boss_pos =
    let next_tile =
      Board.get_tile_c (get_closest_pos p_pos boss_pos) board
    in
    match get_tile_type next_tile with
    | Entrance _ | Exit _ | Wall -> boss_pos
    | Pipe pipe -> get_pipe_end_of_tile next_tile
    | Coin | Empty | Item _ -> get_closest_pos p_pos boss_pos
  in
  { b with current_tile = get_tile_c new_boss_pos board }

let make_boss_state x y tile_type health =
  let tile = make_tile Empty (make_coord x y) in
  { current_tile = tile; health }
