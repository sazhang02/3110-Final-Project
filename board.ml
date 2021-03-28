type tile_type = Wall of wall | Pipe of pipe | Empty

type tile = {tile_type : tile_type; is_obstacle : bool}

type t = tile array

let blank = {tile_type = Empty; is_obstacle = false}

let create_default dimx dimy = Array.make (dimx * dimy) blank

(** [index_of_coord] is the index in the array with width [dimx] of a 
    coordinate [coord]
    Example:
    10 11 12 13 14
    5  6  7  8  9
    0  1  2  3  4
    (0,0) is 0; (1,0) is 1; (0,1) is 5; (2,2) is 12
   *)
let index_of_coord dimx coord = match coord with (x,y) -> x + dimx * y

(** [change_to_wall] is the list of tiles that are walls *)
let change_to_wall dimx walls = 
   List.map (fun _ -> 
      match wall.coord with
      (x, y) -> {tile_type = Wall () (* set to wall *); is_obstacle = true})
   walls

(** [replace i t] replaces the tile at index [i] in layout [t] with a wall. *)
let replace index t = t.(index) <- {tile_type = Wall () (* wall *); is_obstacle = true}

let create_walls dimx dimy wall_lst = Array.make (dimx * dimy) blank |> 
   


