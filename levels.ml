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

let tiles_of_json j = failwith "Unimplemented"

let from_json j =
  {
    levels = j |> member "rooms" |> to_list |> List.map level_of_json;
    board = ();
  }

let rec level_from_id lev = failwith "unimpl"

(* let rec map_room room_id room_list f = match room_list with | [] ->
   raise (UnknownRoom room_id) | h :: t -> if h.id = room_id then f h
   else map_room room_id t f *)

let entrance_pipe (levels : t) (id : level_id) : tile =
  failwith "Unimplemented"

let exit_pipe (levels : t) (id : level_id) : tile =
  failwith "Unimplemented"

let next_level (levels : t) (id : level_id) : level_id =
  failwith "Unimplemented"

let prev_level (levels : t) (id : level_id) : level_id =
  failwith "Unimplemented"
