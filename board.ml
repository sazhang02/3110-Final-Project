type pipe = unit (* TODO replace with pipe *)

type coord = {
  x : int;
  y : int;
}

type room = {
  bl_start : coord;
  (* bottom left*)
  tr_end : coord; (* top right *)
}

type tile_type =
  | Wall
  | Pipe of pipe
  | Entrance
  | Exit
  | Empty

type tile = {
  coords : coord;
  tile_type : tile_type;
}

type t = tile array

let get_x pos = pos.x

let get_y pos = pos.y

let dimx = 16

let dimy = 16

let get_tile_coords tile = tile.coords

let blank = { coords = { x = 0; y = 0 }; tile_type = Wall }

let index_of_coord dimx coord =
  match coord with { x; y } -> x + (dimx * y)

let coord_of_index dimx index = { x = index mod dimx; y = index / dimx }

(** [replace_empty i t] replaces the tile at index [i] in layout [t]
    with an empty tile. *)
let replace_empty index t =
  t.(index) <- { coords = coord_of_index dimx index; tile_type = Empty }

let set_tile tile t = t.(index_of_coord dimx tile.coords) <- tile

let get_tile i (t : t) = t.(i)

let get_tile_c (coord : coord) (t : t) = t.(index_of_coord dimx coord)

let get_size (t : t) = Array.length t

let room_of_coords bottom_left_start top_right_end =
  { bl_start = bottom_left_start; tr_end = top_right_end }

let make_room room t =
  for i = room.bl_start.x to room.tr_end.x do
    for j = room.bl_start.y to room.tr_end.y do
      let coord = { x = i; y = j } in
      replace_empty (index_of_coord dimx coord) t
    done
  done;
  t

let rec make_rooms_board rooms board =
  match rooms with
  | [] -> board
  | h :: t -> make_rooms_board t (make_room h board)

let array_tester t =
  for i = 0 to Array.length t - 1 do
    match t.(i).tile_type with
    | Entrance -> print_endline "entrance"
    | Exit -> print_endline "exit"
    | Empty -> ()
    | Wall -> ()
    | Pipe _ -> ()
  done

(* (* [create_default] is a level with dimensions x by y without any
   floors or pipes. *) let default = let board = Array.make (dimx *
   dimy) blank in for i = 0 to Array.length board - 1 do board.(i) <- {
   coords = coord_of_index dimx i; tile_type = Wall } done; board *)

let make_board entrance exit rooms =
  let board =
    make_rooms_board rooms
      (let def = Array.make (dimx * dimy) blank in
       for i = 0 to Array.length def - 1 do
         def.(i) <- { coords = coord_of_index dimx i; tile_type = Wall }
       done;
       def)
  in
  set_tile entrance board;
  set_tile exit board;
  board
