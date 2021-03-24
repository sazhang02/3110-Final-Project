(** Representation of levels data*)

(** The abstract type of values representing levels.*)
type t

(** The type representing a position in a level. *)
type pos = int * int

(** The type of level identifier. *)
type level_id = string

(** Raised when an unknown level is met. *)
exception UnknownLevel of level_id

val from_json : Yojson.Basic.t -> t

(** [entrance_pipe] is the position [pos] of the entrance pipe of the level [t]. 
*)
val entrance_pipe : t-> pos

(** [exit_pipe] is the position [pos] of the exit pipe of the level [t]. *)
val exit_pipe : t -> pos

(** [next_level] is the [level_id] of the level to which the level [t] exits.
  Raises [UnkownLevel] if [t] is the last level.
*)
val next_level : t -> level_id

(** [prev_level] is the [level_id] of the level from which the level [t] enters.
  Raises [UnknownLevel] if [t] is the starting level.
*)
val prev_level : t -> level_id