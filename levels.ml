type level_id = int

type pos = int * int

type level = {
  level_id : level_id;
  entrance : pos;
  exit : pos;
  exit_id : level_id;
}

type t = { levels : level list }

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
    entrance = j |> member "entrance" |> to_string |> to_tuple;
    exit = j |> member "exit" |> to_string |> to_tuple;
    exit_id = j |> member "exit_id" |> to_int;
  }

let from_json j =
  { levels = j |> member "rooms" |> to_list |> List.map level_of_json }

let entrance_pipe (levels : t) (id : level_id) : pos =
  failwith "Unimplemented"

let exit_pipe (levels : t) (id : level_id) : pos =
  failwith "Unimplemented"

let next_level (levels : t) (id : level_id) : level_id =
  failwith "Unimplemented"

let prev_level (levels : t) (id : level_id) : level_id =
  failwith "Unimplemented"
