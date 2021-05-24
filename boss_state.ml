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

let decrease_health b amount : b =
  let new_health = b.health - amount in
  if new_health > 0 then { b with health = new_health }
  else { b with health = 0 }

let get_distance p_pos b_tile : float =
  let b_pos = get_tile_coords b_tile in
  let player_x = get_x p_pos |> float_of_int in
  let player_y = get_y p_pos |> float_of_int in
  let boss_x = get_x b_pos |> float_of_int in
  let boss_y = get_y b_pos |> float_of_int in
  sqrt (((player_x -. boss_x) ** 2.) +. ((player_y -. boss_y) ** 2.))

let min_distance_sort distances =
  List.sort (fun x y -> Float.compare (fst x) (fst y)) distances

let get_distances p_pos b_pos bt =
  let up_tile =
    get_tile_c (make_coord (get_x b_pos) (get_y b_pos + 1)) bt
  in
  let up_distance = get_distance p_pos up_tile in
  let down_tile =
    get_tile_c (make_coord (get_x b_pos) (get_y b_pos - 1)) bt
  in
  let down_distance = get_distance p_pos down_tile in
  let left_tile =
    get_tile_c (make_coord (get_x b_pos - 1) (get_y b_pos)) bt
  in
  let left_distance = get_distance p_pos left_tile in
  let right_tile =
    get_tile_c (make_coord (get_x b_pos + 1) (get_y b_pos)) bt
  in
  let right_distance = get_distance p_pos right_tile in
  [
    (up_distance, up_tile);
    (down_distance, down_tile);
    (left_distance, left_tile);
    (right_distance, right_tile);
  ]

let get_closest_pos p_pos b_pos (bt : Board.t) : Board.coord =
  let distances = get_distances p_pos b_pos bt |> min_distance_sort in
  let next_tile = snd (List.hd distances) in
  match get_tile_type next_tile with
  | Empty | Coin | Item _ | Pipe _ -> get_tile_coords next_tile
  | _ -> get_tile_coords (snd (List.nth distances 1))

let move_boss (p_pos : Board.coord) b board : b =
  let boss_pos = get_current_pos b in
  let new_boss_pos =
    let next_tile =
      Board.get_tile_c (get_closest_pos p_pos boss_pos board) board
    in
    match get_tile_type next_tile with
    | Entrance _ | Exit _ | Wall -> boss_pos
    | Pipe pipe -> get_pipe_end_of_tile next_tile
    | Coin | Empty | Item _ -> get_closest_pos p_pos boss_pos board
  in
  { b with current_tile = get_tile_c new_boss_pos board }

let make_boss_state x y tile_type health =
  let tile = make_tile Empty (make_coord x y) in
  { current_tile = tile; health }
