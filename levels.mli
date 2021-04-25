(** Representation of levels data*)
open Board

(** The abstract type of values representing levels.*)
type t

(**The type representing the position of a tile*)
type pos = Board.coord

(** The type of level identifier. Each level's [level_id] is unique.*)
type level_id = int

(** The [pos] of [tile]*)
val get_pos : tile -> pos

val get_tile_type : Board.tile -> Board.tile_type

(*ADDED LEVEL TYPE and getter*)
type level

val get_levels : t -> level list

(** Raised when an unknown level is met. *)
exception UnknownLevel of level_id

(** Raised when an invalid tile is met.*)
exception InvalidTile of pos

(** [from_json j] is the set of levels that [j] represents. Requires:
    [j] is a valid json representation.*)
val from_json : Yojson.Basic.t -> t

(** [entrance_pipe levels lev] a [pipe] with position [pos] of the
    entrance pipe of the level [lev] in levels [levels]. *)
val entrance_pipe : t -> level_id -> tile

(** [exit_pipe levels lev] is a [tile] with position [pos] of the exit
    pipe of the level [lev] in levels [levels]. Raises [InvalidTile] if
    the [lev] is the last level (i.e. There is no exit) *)
val exit_pipe : t -> level_id -> tile

(** [next_level levels id] is the [level_id] of the level to which the
    level [lev] exits. Raises [UnkownLevel] if [lev] is the last level. *)
val next_level : t -> level_id -> level_id

(** [prev_level levelsxit pipes associated with th id] is the [level_id]
    of the level from which the level [lev] enters. Raises
    [UnknownLevel] if [lev] is the starting level. *)
val prev_level : t -> level_id -> level_id

(** [make_board levels id] creates a 16x16 board of [Board.t] type with
    the entrance and ee [id] in [levels]. *)
val make_board : t -> level_id -> Board.t
