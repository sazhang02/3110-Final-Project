type b

val init_state : Board.coord -> Board.t -> b

val get_current_tile : b -> Board.tile

val get_current_pos : b -> Board.coord

val get_health : b -> int

val set_health : b -> int -> b

val decrease_health : b -> int -> b

val move_boss : Board.coord -> b -> Board.t -> b

(** [make_boss_state x y tile_type health] is the boss state with [tile]
    at coordinate [(x, y)] and [health] is its health level. *)
val make_boss_state : int -> int -> Board.tile_type -> int -> b
