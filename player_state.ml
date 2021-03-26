open Yojson.Basic.Util
open Levels

type level_id = Levels.level_id

type position = int * int

type p = {
  current_pos : position;
  current_level : level_id;
}

let init_state = failwith "Unimplemented"

let get_current_level (ps : p) = ps.current_level

let get_current_pos (ps : p) = ps.current_pos

type result =
  | Legal of p
  | Illegal
