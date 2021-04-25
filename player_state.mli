(* The abstract type representing player state *)
type p

(* The type of the level identifier. Each level's [level_id] is unique. *)
type level_id = int

(* The abstract type representing player's intended move *)
type move

(* The abstract type of player's position: with x and y coordinates in
   the GUI grid *)
(* type position *)

(** Raised when an unknown position is met. *)

(* exception UnknownPosition of position *)

(** [init_state a] is the initial state of the game when playing in each
    level. In that state the character is currently located in the
    starting position, and they have visited only that level. *)
val init_state : Levels.t -> Board.t -> p

(** [get_current_level p] is the identifier of the level in which the
    adventurer currently is located in state [ps]. *)
val get_current_level : p -> Levels.level_id

val get_current_tile : p -> Board.tile

(** [get current_pos p] is the position in which the character currently
    is located in state [p]. *)
val get_current_pos : p -> Board.coord

val get_coins : p -> int

(* val get_image : p -> Graphics.image *)

(* val update_pos : char -> p -> Gui.coords *)

val update : char -> p -> Levels.t -> Board.t -> p

(** The type representing the result of an attempted movement. *)

(* type result = | Legal of p | Illegal *)
