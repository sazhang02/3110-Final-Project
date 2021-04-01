type wall = unit (* TODO replace with wall *)

type pipe = unit (* TODO replace with pipe *)

type coord = {
  x : int;
  y : int;
}

type tile_type =
  | Wall of wall
  | Pipe of pipe
  | Empty

type tile = {
  coords : coord;
  tile_type : tile_type;
  is_obstacle : bool;
}

type t = tile array

let blank =
  { coords = { x = 0; y = 0 }; tile_type = Empty; is_obstacle = false }

(** [create_default x y] makes a level layout with blank tiles*)
let create_default dimx dimy = Array.make (dimx * dimy) blank

(** [index_of_coord] is the index in the array with width [dimx] of a
    coordinate [coord] Example: 10 11 12 13 14 5 6 7 8 9 0 1 2 3 4 (0,0)
    is 0; (1,0) is 1; (0,1) is 5; (2,2) is 12; (4,2) is 14*)
let index_of_coord dimx coord =
  match coord with { x; y } -> x + (dimx * y)

let coord_of_index dimx index = { x = index mod dimx; y = index / dimx }

(** [change_to_wall] is the list of tiles that are walls *)

(*let change_to_wall dimx walls = List.map (fun w -> match w.coords with
  | x, y -> { tile_type = Wall () (* set to wall *); is_obstacle = true
  }) walls *)

(** [replace i t] replaces the tile at index [i] in layout [t] with a
    wall. *)
let replace index t dimx =
  t.(index) <-
    {
      coords = coord_of_index index dimx;
      tile_type = Wall () (* wall *);
      is_obstacle = true;
    }

(** [create_walls x y walls] creates a level of dimensions [x] by [y]
    with walls [walls]*)

(*let create_walls dimx dimy wall_lst = Array.make (dimx * dimy) blank
  |> *)
