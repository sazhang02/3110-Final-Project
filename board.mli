(* The abstract type representing the level layout. *)
type coord

type color =
  | Green
  | Red
  | Blue

type orientation =
  | Left
  | Right
  | Up
  | Down

type pipe

type tile_type =
  | Wall
  | Pipe of pipe
  | Entrance
  | Exit
  | Empty

type room

(** [index_of_coord] is the index in the array with width [dimx] of a
    coordinate [coord]. Example: In a 5x3 level, (0,0) is 0; (1,0) is 1;
    (0,1) is 5; (2,2) is 12; (4,2) is 14*)
val index_of_coord : int -> coord -> int

(** The type representing a tile in a level. A tile has a position and a
    type (ie wall, pipe, etc). *)
type tile

(** The type representing a board. *)
type t

(** [get_x pos] is the x coordinate. *)
val get_x : coord -> int

(** [get_y pos] is the y coordinate. *)
val get_y : coord -> int

(**[make_coord x y] is a [coord] at [(x, y)].*)
val make_coord : int -> int -> coord

(** [get_tile i t] is the tile at index [i] of level [t]. *)
val get_tile : int -> t -> tile

(** [make_tile i tt] is a tile with coord [i] and tile_type [tt]*)
val make_tile : coord -> tile_type -> tile

(** [get_tile_coords t] is the coordinates of tile [t]. *)
val get_tile_coords : tile -> coord

(** [get_tile_type t] is the tile_type of tile [t]*)
val get_tile_type : tile -> tile_type

(** [get_tile_c c t] is the tile at coordinates [c] of level [t]. *)
val get_tile_c : coord -> t -> tile

(** [get_size t] is the number of tiles in level [t]. *)
val get_size : t -> int

(** [set_tile tile x t] adds [tile] in level [t]. *)
val set_tile : tile -> t -> unit

(** [get_pipe_end p] is the end coordinate pair of pipe [p]. *)
val get_pipe_end : pipe -> coord

(** [get_pipe_color p] is the color of pipe [p]. *)
val get_pipe_color : pipe -> color

(** [get_pipe_oreintation p] is the direction pipe [p] is facing. *)
val get_pipe_orientation : pipe -> orientation

(* val make_pipe *)
val make_pipe_tile_pair : coord -> color -> orientation -> tile list

(** [room_of_coords start_coord end_coord] makes a [room] type with
    [start_coord] and [end_coord]. *)

val room_of_coords : coord -> coord -> room

(** [make_board en ex lst t] makes a level with entrance [en], exit
    [ex], and rooms [lst]. *)
val make_board : tile -> tile -> room list -> t

val alla_board : tile -> tile -> room list -> tile list -> t
