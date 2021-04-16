type wall = unit (* TODO replace with wall *)

type pipe = unit (* TODO replace with pipe *)

type coord = {
  x : int;
  y : int;
}

type tile_type =
  | Wall of wall
  | Pipe of pipe
  | Entrance
  | Exit
  | Empty

type tile = {
  coords : coord;
  tile_type : tile_type;
}

type t = tile array

let get_tile_array t = t

let blank = { coords = { x = 0; y = 0 }; tile_type = Empty }

(** [create_default x y] makes a level layout with blank tiles*)

let index_of_coord dimx coord =
  match coord with { x; y } -> x + (dimx * y)

let coord_of_index dimx index = { x = index mod dimx; y = index / dimx }

let create_default dimx dimy =
  let default = Array.make (dimx * dimy) blank in
  for i = 0 to Array.length default - 1 do
    default.(i) <- { coords = coord_of_index dimx i; tile_type = Empty }
  done;
  default

(** [change_to_wall] is the list of tiles that are walls *)

(*let change_to_wall dimx walls = List.map (fun w -> match w.coords with
  | x, y -> { tile_type = Wall () (* set to wall *); is_obstacle = true
  }) walls *)

(** [replace i t] replaces the tile at index [i] in layout [t] with a
    wall. *)
let replace_wall index t dimx =
  t.(index) <-
    {
      coords = coord_of_index index dimx;
      tile_type = Wall () (* wall *);
    }

let set_tile tile dimx t = t.(index_of_coord dimx tile.coords) <- tile

let get_tile index (t : t) = t.(index)

(** [tiles_of_type t s] is a list of tiles *)

let make_board dimx dimy entrance exit =
  let board = create_default dimx dimy in
  set_tile entrance dimx board;
  set_tile exit dimx board;
  board
