(** Representation of boss state data *)

(** The abstract type representing boss state *)
type b

(** [init_state entrance_pos bt] is the initial state of the boss in the
    game. In that state the boss is on the starting tile and has full
    health. *)
val init_state : Board.coord -> Board.t -> b

(** [get_current_tile b] is the tile in which the boss is currently
    located in state [b]. *)
val get_current_tile : b -> Board.tile

(** [get current_pos b] is the position coordinate in which the boss is
    currently located in state [b]. *)
val get_current_pos : b -> Board.coord

(** [get_health b] is the health that the boss currently has in state
    [b]. *)
val get_health : b -> int

(** [set_health b health] sets the health of the boss to [health] in
    state [b]. *)
val set_health : b -> int -> b

(** [decrease_health b damage] decreases the health of the boss by
    [damage] in state [b]. *)
val decrease_health : b -> int -> b

(** [move_boss p_pos b board] moves the boss depending on the player
    position [p_pos] and getes a new boss position and uses [board] to
    update the boss state [b]. *)
val move_boss : Board.coord -> b -> Board.t -> b

(** [make_boss_state x y tile_type health] is the boss state with [tile]
    at coordinate [(x, y)] and [health] is its health level. *)
val make_boss_state : int -> int -> Board.tile_type -> int -> b
