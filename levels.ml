open Board

type level_id = int

type pos = Board.coord

type tile = Board.tile

type board = Board.t

type room = Board.room

type pipe_info = {
  pos : pos;
  color : Board.color;
  orientation : Board.orientation;
}

(* { bottom_left_start : coord; top_right_end : coord; } *)

type level = {
  level_id : level_id;
  entrance_pos : pos;
  exit_pos : pos;
  exit_id : level_id;
  rooms : room list;
  pipes : pipe_info list;
}

type t = { levels : level list }

exception UnknownLevel of level_id

exception InvalidTile of pos

open Yojson.Basic.Util

let get_pos tile = tile.coords

let get_tile_type tile = tile.tile_type

let get_levels t = t.levels

let pos_of_json_tile j =
  { x = j |> member "x" |> to_int; y = j |> member "y" |> to_int }

let room_of_json j =
  let bot = j |> member "start" |> pos_of_json_tile in
  let top = j |> member "end" |> pos_of_json_tile in
  Board.room_of_coords bot top

let color_of_string str =
  match str with
  | "Red" -> Red
  | "Blue" -> Blue
  | "Green" -> Green
  | _ -> failwith "Invalid Color"

let orient_of_string str =
  match str with
  | "Up" -> Up
  | "Down" -> Down
  | "Left" -> Left
  | "Right" -> Right
  | _ -> failwith "Invalid Orientation"

let pipes_of_json j =
  let pos =
    { x = j |> member "x" |> to_int; y = j |> member "y" |> to_int }
  in
  {
    pos;
    color = j |> member "color" |> to_string |> color_of_string;
    orientation =
      j |> member "orientation" |> to_string |> orient_of_string;
  }

let level_of_json j =
  {
    level_id = j |> member "id" |> to_int;
    entrance_pos = j |> member "entrance" |> pos_of_json_tile;
    exit_pos = j |> member "exit" |> pos_of_json_tile;
    exit_id = j |> member "exit_id" |> to_int;
    rooms = j |> member "rooms" |> to_list |> List.map room_of_json;
    pipes = j |> member "pipes" |> to_list |> List.map pipes_of_json;
  }

let from_json j =
  { levels = j |> member "levels" |> to_list |> List.map level_of_json }

(** [map_level id t f] is [f] applied to the level that [id] corresponds
    to in [t]*)
let rec map_level id level_list f =
  match level_list with
  | [] -> raise (UnknownLevel id)
  | h :: t -> if h.level_id = id then f h else map_level id t f

let to_tile levels id f tile_type =
  let pos = map_level id levels.levels f in
  { coords = pos; tile_type }

let entrance_pos level = level.entrance_pos

let entrance_pipe (levels : t) (id : level_id) : tile =
  to_tile levels id entrance_pos Entrance

let exit_pos level = level.exit_pos

let exit_pipe (levels : t) (id : level_id) : tile =
  let exit_tile = to_tile levels id exit_pos Exit in
  (* if exit_tile.coords.x = -1 || exit_tile.coords.y = -1 then raise
     (InvalidTile exit_tile.coords) else *)
  exit_tile

(* let make_board entrance exit = Board.t *)

let exit_id level = level.exit_id

let check_level_validity id =
  if id = -1 then raise (UnknownLevel id) else id

let next_level (levels : t) (id : level_id) : level_id =
  map_level id levels.levels exit_id |> check_level_validity

let level_id level = level.level_id

let prev_level (levels : t) (id : level_id) : level_id =
  map_level id levels.levels level_id - 1 |> check_level_validity

let rooms_list level = level.rooms

let pipe_info_to_tile_pair (pipe_info : pipe_info) : tile list =
  Board.make_pipe_tile_pair pipe_info.pos pipe_info.color
    pipe_info.orientation

let pipes_list (pipe_info_lst : pipe_info list) : Board.tile list =
  let pipes =
    List.map pipe_info_to_tile_pair pipe_info_lst |> List.flatten
  in
  print_endline (List.length pipes |> string_of_int);
  pipes

let pipes_info_list level = level.pipes

let make_board levels id =
  let entr = entrance_pipe levels id in
  let exit = exit_pipe levels id in
  (*if exit is negative, make a board with no exit??? *)
  let rooms = map_level id levels.levels rooms_list in
  let pipes =
    map_level id levels.levels pipes_info_list |> pipes_list
  in
  alla_board entr exit rooms pipes
