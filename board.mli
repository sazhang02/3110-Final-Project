(* The abstract type representing the level layout. *)
type t

(** The type representing a tile in a level. A tile has a position and a type (ie wall, pipe, etc). *)
type tile = {
  tile_type : unit; (**TODO: change when we implement walls/pipes*) 
  is_obstacle : bool;
  }

(** [create_default x y] makes a level with dimensions x by y without any walls 
    or pipes. *)
val create_default : int -> int -> t

(** [create_walls] makes a level with walls *)
val create_walls : int -> int -> 'a -> t