(* The abstract type representing the level layout. *)

type wall

type pipe

type tile_type =
  | Wall of wall
  | Pipe of pipe
  | Entrance
  | Exit
  | Empty

type coord = {
  x : int;
  y : int;
}

(** [index_of_coord] is the index in the array with width [dimx] of a
    coordinate [coord] Example: In a 5x3 level, (0,0) is 0; (1,0) is 1;
    (0,1) is 5; (2,2) is 12; (4,2) is 14*)
val index_of_coord : int -> coord -> int

(** [coord_of_index] is the coordinate of the index in the array with
    width [dimx] of a coordinate [coord] Example: In a 5x3 level, (0,0)
    is 0; (1,0) is 1; (0,1) is 5; (2,2) is 12; (4,2) is 14*)
val index_of_coord : int -> coord -> int

(** The type representing a tile in a level. A tile has a position and a
    type (ie wall, pipe, etc). *)
type tile = {
  coords : coord;
  tile_type : tile_type;
}

type t

(** [get_x pos] is the x coordinate. *)
val get_x : coord -> int

(** [get_y pos] is the y coordinate. *)
val get_y : coord -> int

(** [get_tile i t] is the tile at index [i] of level [t]. *)
val get_tile : int -> t -> tile

(** [get_size t] is the number of tiles in level [t]. *)
val get_size : t -> int

(** [set_tile tile x t] adds [tile] in level [t] with dimx [x]. *)
val set_tile : tile -> int -> t -> unit

(** [create_default x y] makes a level with dimensions x by y without
    any walls or pipes. *)
val create_default : int -> int -> t

(** [make_board x y en ex t] makes a level with dimensions [x] by [y]
    with entrance [en] and exit [ex]. *)
val make_board : int -> int -> tile -> tile -> t

(* [create_walls] makes a level with walls *)

(* val create_walls : int -> int -> 'a -> t *)
