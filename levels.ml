type level_id = int

type pos = int * int

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
  exit_id : level_id; (* objects : tile list; *)
}

type t = {
  levels : level list;
  board : board;
}

exception UnknownLevel of level_id

open Yojson.Basic.Util

let list_to_tuple = function [] -> (0, 0) | h :: t -> (h, List.hd t)

let to_tuple str =
  String.length str - 2
  |> String.sub str 1
  |> String.split_on_char ','
  |> List.map String.trim |> List.map int_of_string |> list_to_tuple

let level_of_json j =
  {
    level_id = j |> member "id" |> to_int;
    entrance_pos = j |> member "entrance" |> to_string |> to_tuple;
    exit_pos = j |> member "exit" |> to_string |> to_tuple;
    exit_id = j |> member "exit_id" |> to_int;
  }

let from_json j =
  {
    levels = j |> member "levels" |> to_list |> List.map level_of_json;
    board = ();
  }

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

(**TODO: change tile_type to pipe. Decide how to handle final level when
   there is no exit. *)
let exit_pipe (levels : t) (id : level_id) : tile =
  to_tile levels id exit_pos ()

(** empty str means exit does not exist????*)
let next_level (levels : t) (id : level_id) : level_id =
  failwith "Unimplemented"

let prev_level (levels : t) (id : level_id) : level_id =
  failwith "Unimplemented"
