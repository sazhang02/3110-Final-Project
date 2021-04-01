open Board

type level_id = int

(* type pos = { x : int; y : int; } *)

type pos = Board.coord

type tile = {
  pos : pos;
  tile_type : unit;  (**TODO: change to pipe/wall type*)
}

(**TODO: replace with Board type*)
type board = unit

type level = {
  level_id : level_id;
  entrance_pos : pos;
  exit_pos : pos;
  exit_id : level_id;
  (* objects : tile list; *)
  board : board;
}

type t = { levels : level list }

exception UnknownLevel of level_id

exception InvalidTile of pos

open Yojson.Basic.Util

let get_pos tile = tile.pos

let get_tile_type tile = tile.tile_type

let pos_of_json_tile j =
  { x = j |> member "x" |> to_int; y = j |> member "y" |> to_int }

(**TODO: change board type*)
let level_of_json j =
  {
    level_id = j |> member "id" |> to_int;
    entrance_pos = j |> member "entrance" |> pos_of_json_tile;
    exit_pos = j |> member "exit" |> pos_of_json_tile;
    exit_id = j |> member "exit_id" |> to_int;
    board = ();
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
  { pos; tile_type }

let entrance_pos level = level.entrance_pos

(**TODO: change tile_type to pipe *)
let entrance_pipe (levels : t) (id : level_id) : tile =
  to_tile levels id entrance_pos ()

let exit_pos level = level.exit_pos

(**TODO: change tile_type to pipe. *)
let exit_pipe (levels : t) (id : level_id) : tile =
  let exit_tile = to_tile levels id exit_pos () in
  if exit_tile.pos.x = -1 || exit_tile.pos.y = -1 then
    raise (InvalidTile exit_tile.pos)
  else exit_tile

let exit_id level = level.exit_id

let check_level_validity id =
  if id = -1 then raise (UnknownLevel id) else id

let next_level (levels : t) (id : level_id) : level_id =
  map_level id levels.levels exit_id |> check_level_validity

let level_id level = level.level_id

let prev_level (levels : t) (id : level_id) : level_id =
  map_level id levels.levels level_id - 1 |> check_level_validity
