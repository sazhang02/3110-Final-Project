type b

val init_state : Board.t -> b

val get_current_tile : b -> Board.tile

val get_current_pos : b -> Board.coord

val move_boss : Board.coord -> b -> Board.t -> b
