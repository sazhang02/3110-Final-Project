type coord

type color =
  | Green
  | Red
  | Gold
  | Blue
  | Black

type orientation =
  | Left
  | Right
  | Up
  | Down

type pipe

type tile_type =
  | Wall
  | Pipe of pipe
  | Entrance of orientation
  | Exit of orientation
  | Empty
  | Coin

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

(** [get_tile i t] is the tile at index [i] of board [t]. *)
val get_tile : int -> t -> tile

(** [make_tile i tt] is a tile with coord at [i] and tile_type [tt]*)
val make_tile : tile_type -> coord -> tile

(** [get_tile_coords t] is the coordinates of tile [t]. *)
val get_tile_coords : tile -> coord

(** [get_tile_type t] is the tile_type of tile [t]*)
val get_tile_type : tile -> tile_type

(** [get_tile_c c t] is the tile at coordinates [c] of board [t]. *)
val get_tile_c : coord -> t -> tile

(** [get_size t] is the number of tiles in level [t]. *)
val get_size : t -> int

(** [set_tile tile x t] replaces the existing tile is [t] with [tile]. *)
val set_tile : tile -> t -> unit

(** [get_pipe_end_of_tile t] is the end coordinate pair of pipe in tile
    [t]. *)
val get_pipe_end_of_tile : tile -> coord

(** [get_pipe_color p] is the color of pipe [p]. *)
val get_pipe_color : pipe -> color

(** [get_pipe_oreintation p] is the direction pipe [p] is facing. val
    get_pipe_orientation : pipe -> orientation *)

(** [get_tile_orientation t] is the orientation of the tile [t].
    Requires: tile has tile_type [Entrance], [Exit], or [Pipe]. *)
val get_tile_orientation : tile -> orientation

(** [make_pipe_tile e c o] is the tile at coordinate [e] and tile type
    pipe with color [c] and orientation [o]. *)
val make_pipe_tile : coord -> color -> orientation -> tile

(** [room_of_coords s e] makes a [room] whose bottom-left coordinate is
    [s] and top-right coordinate is [e]. *)
val room_of_coords : coord -> coord -> room

(** [random_item t] is the board [t] with a randomly placed [Wall] tile. *)
val random_item : t -> t

(** [alla_board en ex rooms pipes coins] makes a level with "alla" the
    properties: entrance [en], exit [ex], [rooms], [pipes], and [coins]. *)
val alla_board :
  tile -> tile -> room list -> tile list -> tile list -> t

(** [board_to_string t] is the string representation of board [t]. *)
val board_to_string : t -> string
