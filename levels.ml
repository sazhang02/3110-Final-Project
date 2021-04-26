open Board

type level_id = int

type pos = Board.coord

type entr_ex_info = {
  pos : Board.coord;
  orientation : Board.orientation;
}

type pipe_info = {
  pos : Board.coord;
  color : Board.color;
  orientation : Board.orientation;
}

(* type coin = { pos : Board.coord } *)

type level = {
  level_id : level_id;
  entrance_pos : entr_ex_info;
  exit_pos : entr_ex_info;
  exit_id : level_id;
  rooms : room list;
  pipes : pipe_info list;
  coins : pos list;
}

type t = { levels : level list }

exception UnknownLevel of level_id

exception InvalidTile of pos

open Yojson.Basic.Util

let get_pos tile = get_tile_coords tile

let get_tile_type tile = get_tile_type tile

let get_levels t = t.levels

let pos_of_json j =
  let x = j |> member "x" |> to_int in
  let y = j |> member "y" |> to_int in
  make_coord x y

let room_of_json j =
  let bot = j |> member "start" |> pos_of_json in
  let top = j |> member "end" |> pos_of_json in
  room_of_coords bot top

let color_of_string str =
  match str with
  | "Red" -> Red
  | "Gold" -> Gold
  | "Green" -> Green
  | "Blue" -> Blue
  | _ -> failwith "Invalid Color"

let orient_of_string str =
  match str with
  | "Up" -> Up
  | "Down" -> Down
  | "Left" -> Left
  | "Right" -> Right
  | _ -> failwith "Invalid Orientation"

let orientation_of_json j =
  j |> member "orientation" |> to_string |> orient_of_string

let pipes_of_json j =
  let x = j |> member "x" |> to_int in
  let y = j |> member "y" |> to_int in
  let pos = make_coord x y in
  {
    pos;
    color = j |> member "color" |> to_string |> color_of_string;
    orientation = orientation_of_json j;
  }

let entr_ex_of_json j =
  let x = j |> member "x" |> to_int in
  let y = j |> member "y" |> to_int in
  let pos = make_coord x y in
  { pos; orientation = orientation_of_json j }

let level_of_json j =
  {
    level_id = j |> member "id" |> to_int;
    entrance_pos = j |> member "entrance" |> entr_ex_of_json;
    exit_pos = j |> member "exit" |> entr_ex_of_json;
    exit_id = j |> member "exit_id" |> to_int;
    rooms = j |> member "rooms" |> to_list |> List.map room_of_json;
    pipes = j |> member "pipes" |> to_list |> List.map pipes_of_json;
    coins = j |> member "coins" |> to_list |> List.map pos_of_json;
  }

let from_json j =
  { levels = j |> member "levels" |> to_list |> List.map level_of_json }

(** [map_level id t f] is [f] applied to the level that [id] corresponds
    to in [t]*)
let rec map_level id level_list f =
  match level_list with
  | [] -> raise (UnknownLevel id)
  | h :: t -> if h.level_id = id then f h else map_level id t f

let to_orientation levels id f = map_level id levels.levels f

let to_tile levels id f tile_type =
  let pos = map_level id levels.levels f in
  make_tile pos tile_type

let entrance_pos level = level.entrance_pos.pos

let entrance_orientation level = level.entrance_pos.orientation

let entrance_pipe (levels : t) (id : level_id) : tile =
  let o = to_orientation levels id entrance_orientation in
  to_tile levels id entrance_pos (Entrance o)

let exit_pos level = level.exit_pos.pos

let exit_orientation level = level.exit_pos.orientation

let exit_pipe (levels : t) (id : level_id) : tile =
  let o = to_orientation levels id exit_orientation in
  let exit_tile = to_tile levels id exit_pos (Exit o) in
  (* if exit_tile.coords.x = -1 || exit_tile.coords.y = -1 then raise
     (InvalidTile exit_tile.coords) else *)
  exit_tile

let exit_id level = level.exit_id

let check_level_validity id =
  if id < 0 then raise (UnknownLevel id) else id

let next_level (levels : t) (id : level_id) : level_id =
  map_level id levels.levels exit_id |> check_level_validity

let level_id level = level.level_id

let prev_level (levels : t) (id : level_id) : level_id =
  map_level id levels.levels level_id - 1 |> check_level_validity

let rooms_list level = level.rooms

let pipe_info_to_tile (pipe_info : pipe_info) : tile =
  make_pipe_tile pipe_info.pos pipe_info.color pipe_info.orientation

let pipes_list (pipe_info_lst : pipe_info list) : Board.tile list =
  let pipes = List.map pipe_info_to_tile pipe_info_lst in
  print_endline (List.length pipes |> string_of_int);
  pipes

let pipes_info_list level = level.pipes

let coins_list level = level.coins

let make_board levels id =
  let entr = entrance_pipe levels id in
  let exit = exit_pipe levels id in
  (*if exit is negative, make a board with no exit??? *)
  let rooms = map_level id levels.levels rooms_list in
  let pipes =
    map_level id levels.levels pipes_info_list |> pipes_list
  in
  (* let coins = map_level id levels.levels coins_list in *)
  alla_board entr exit rooms pipes
