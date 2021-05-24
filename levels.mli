(** Representation of levels data*)
open Board

(** The abstract type of values representing levels.*)
type t

(**The type representing the position of a tile*)
type pos = Board.coord

(** The type of level identifier. Each level's [level_id] is unique.*)
type level_id = int

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

(** [prev_level levels] is the [level_id] level [lev] enters. Raises
    [UnknownLevel] of [level_id] if [lev] is not a level in [levels]. *)
val prev_level : t -> level_id -> level_id

(**[total_coin_count levels id] is the sum of the number of coins on
   level number [id] and of the coin counts o n previous levels in
   [levels]*)
val coin_count : t -> level_id -> int

(**[is_final levels level_id] is true if [level_id] is the id of the
   last level in [levels], else false*)
val is_final_level : t -> level_id -> bool

(**[final_level_id levels] is the id of the last level in [levels]*)
val final_level_id : t -> level_id

(** [make_board levels id] is a 16x16 board of [Board.t] type with
    information from level number [id] in [levels]. *)
val make_board : t -> level_id -> Board.t

(**[make_all_boards levels] is a set of boards representing [levels] *)
val make_all_boards : t -> Board.t array
