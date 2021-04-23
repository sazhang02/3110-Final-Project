(** Representation of pipe data*)

type c = Green | Blue | Red

(** [relocate p pipe] is the player state after the player moves
   through [pipe]. *) 
val relocate : Player_state.p -> 