(* The abstract type representing player state *)
type p

(* The type of the level identifier. Each level's [level_id] is unique. *)
type level_id = int

(* The abstract type of player's position: with x and y coordinates in
   the GUI grid *)
type position

(** Raised when an unknown position is met. *)
exception UnknownPosition of position

val get_x : position -> int

val get_y : position -> int

(** [init_state a] is the initial state of the game when playing in each
    level. In that state the character is currently located in the
    starting position, and they have visited only that level. *)
val init_state : Levels.t -> Levels.level_id -> Levels.tile

(** [get_current_level p] is the identifier of the level in which the
    adventurer currently is located in state [ps]. *)
val get_current_level : p -> Levels.level_id

(** [get current_pos p] is the position in which the character currently
    is located in state [p]. *)
val get_current_pos : p -> position

val update_pos : char -> p -> position

val update : char -> p -> p

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of p
  | Illegal
