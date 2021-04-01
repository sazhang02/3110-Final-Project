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

let blank = { coords = { x = 0; y = 0 }; tile_type = Empty }

(** [create_default x y] makes a level layout with blank tiles*)
let create_default dimx dimy = Array.make (dimx * dimy) blank

let index_of_coord dimx coord =
  match coord with { x; y } -> x + (dimx * y)

let coord_of_index dimx index = { x = index mod dimx; y = index / dimx }

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

let add_tile tile dimx t = t.(index_of_coord dimx tile.coords) <- tile

let get_tile index (t : t) = t.(index)

(* let tile_to_string tile = match tile with *)

(** [make_board x en ex t] makes a level [t] with dimx [x], entrance
    [en], exit [ex]*)
let make_board dimx entrance exit t =
  add_tile entrance dimx t;
  add_tile exit dimx t

(** [create_walls x y walls] creates a level of dimensions [x] by [y]
    with walls [walls]*)

(*let create_walls dimx dimy wall_lst = Array.make (dimx * dimy) blank
  |> *)
