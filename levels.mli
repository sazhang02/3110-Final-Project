(** Representation of levels data*)

(** The abstract type of values representing levels.*)
type t

(** The type representing a tile in a level. A tile has a position and a
    type (ie wall, pipe, etc). *)
type tile = {
  pos : int * int;
  tile_type : unit;  (**TODO: change when we implement walls/pipes*)
}

(** TODO: replace with board from Board module *)
type board = unit

(** The type of level identifier. Each level's [level_id] is unique.*)
type level_id = int

(** Raised when an unknown level is met. *)
exception UnknownLevel of level_id

(** [from_json j] is the set of levels that [j] represents. Requires:
    [j] is a valid json representation.*)
val from_json : Yojson.Basic.t -> t

(** [entrance_pipe levels lev] a [tile] with position [pos] of the
    entrance pipe of the level [lev] in levels [levels]. *)
val entrance_pipe : t -> level_id -> tile

(** [exit_pipe levels lev] is a [tile] with position [pos] of the exit
    pipe of the level [lev] in levels [levels]. *)
val exit_pipe : t -> level_id -> tile

(** [next_level levels id] is the [level_id] of the level to which the
    level [lev] exits. Raises [UnkownLevel] if [lev] is the last level. *)
val next_level : t -> level_id -> level_id

(** [prev_level levels id] is the [level_id] of the level from which the
    level [lev] enters. Raises [UnknownLevel] if [lev] is the starting
    level. *)
val prev_level : t -> level_id -> level_id
