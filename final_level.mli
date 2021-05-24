(** Executes the final level. An extension of Main. *)

(** [final_level b z p t] runs the the last level of [t] with board [b]
    and player [p]. The GUI window continues at scaling size [z]. *)
val final_level :
  Board.t -> Gui.scaling -> Player_state.p -> Levels.t -> unit
