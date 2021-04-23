(* The abstract type representing the level layout. *)

type pipe

type tile_type =
  | Wall
  | Pipe of pipe
  | Entrance
  | Exit
  | Empty

type coord = {
  x : int;
  y : int;
}

type room (* { bottom_left_start : coord; top_right_end : coord; } *)

(** [index_of_coord] is the index in the array with width [dimx] of a
    coordinate [coord]. Example: In a 5x3 level, (0,0) is 0; (1,0) is 1;
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

(** The type representing a level. *)
type t

(** [get_x pos] is the x coordinate. *)
val get_x : coord -> int

(** [get_y pos] is the y coordinate. *)
val get_y : coord -> int

(** [get_tile i t] is the tile at index [i] of level [t]. *)
val get_tile : int -> t -> tile

(** [get_tile_coords t] is the coordinates of tile [t]. *)
val get_tile_coords : tile -> coord

(** [get_tile_c c t] is the tile at coordinates [c] of level [t]. *)
val get_tile_c : coord -> t -> tile

(** [get_size t] is the number of tiles in level [t]. *)
val get_size : t -> int

(** [set_tile tile x t] adds [tile] in level [t]. *)
val set_tile : tile -> t -> unit

(** [room_of_coords start_coord end_coord] makes a [room] type with
    [start_coord] and [end_coord]. *)

val room_of_coords : coord -> coord -> room

(** [make_board en ex t] makes a level with entrance [en] and exit [ex]. *)
val make_board : tile -> tile -> room list -> t
