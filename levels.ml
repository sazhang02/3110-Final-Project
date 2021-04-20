open Board

type level_id = int

type pos = Board.coord

type tile = Board.tile

type board = Board.t

(**TODO: replace meeee*)
type room = {
  bottom_left_start : coord;
  top_right_end : coord;
}

type level = {
  level_id : level_id;
  entrance_pos : pos;
  exit_pos : pos;
  exit_id : level_id;
  rooms : room list;
}

type t = { levels : level list }

exception UnknownLevel of level_id

exception InvalidTile of pos

open Yojson.Basic.Util

let get_pos tile = tile.coords

let get_tile_type tile = tile.tile_type

let pos_of_json_tile j =
  { x = j |> member "x" |> to_int; y = j |> member "y" |> to_int }

let level_of_json j =
  {
    level_id = j |> member "id" |> to_int;
    entrance_pos = j |> member "entrance" |> pos_of_json_tile;
    exit_pos = j |> member "exit" |> pos_of_json_tile;
    exit_id = j |> member "exit_id" |> to_int;
    rooms = [];
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

(**TODO: change tile_type to pipe *)
let entrance_pipe (levels : t) (id : level_id) : tile =
  to_tile levels id entrance_pos Entrance

let exit_pos level = level.exit_pos

(**TODO: change tile_type to pipe. *)
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

(* let dimx = 16 let dimy = 16 *)

let make_board levels id =
  let entr = entrance_pipe levels id in
  let exit = exit_pipe levels id in
  (*if exit is negative, make a board with no exit *)
  make_board entr exit
