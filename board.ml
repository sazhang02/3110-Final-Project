type coord = {
  x : int;
  y : int;
}

type orientation =
  | Left
  | Right
  | Up
  | Down

type color =
  | Green
  | Red
  | Blue

type room = {
  bl_start : coord;
  (* bottom left*)
  tr_end : coord; (* top right *)
}

type pipe = {
  end_coords : coord;
  orientation : orientation;
  color : color;
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

let make_coord x y = { x; y }

let dimx = 16

let dimy = 16

let get_tile_coords tile = tile.coords

let get_tile_type tile = tile.tile_type

let blank = { coords = { x = 0; y = 0 }; tile_type = Wall }

let index_of_coord dimx coord =
  match coord with { x; y } -> x + (dimx * y)

let coord_of_index dimx index = { x = index mod dimx; y = index / dimx }

(** [replace_empty i t] replaces the tile at index [i] in layout [t]
    with an empty tile. *)
let replace_empty index t =
  t.(index) <- { coords = coord_of_index dimx index; tile_type = Empty }

let make_tile coords tile_type = { coords; tile_type }

let set_tile tile t = t.(index_of_coord dimx tile.coords) <- tile

let get_tile i (t : t) = t.(i)

let get_tile_c (coord : coord) (t : t) = t.(index_of_coord dimx coord)

let get_size (t : t) = Array.length t

let room_of_coords bottom_left_start top_right_end =
  { bl_start = bottom_left_start; tr_end = top_right_end }

let get_pipe_end pipe = pipe.end_coords

let get_pipe_color pipe = pipe.color

let get_pipe_orientation pipe = pipe.orientation

(* let determine_orientation entr = let horizontal_bound = dimx - 1 in
   let vertical_bound = dimy - 1 in match entr with | { x = 0; _} ->
   failwith "left wall" | { x = horizontal_bound; _} -> failwith "right
   wall" | { _; y = 0 } -> failwith "bottom wall" | { _; y =
   vertical_bound } -> failwith "top wall" *)

(** [reflect_green orientation start] is the [coord] in front of the
    pipe's exit. *)
let reflect_green orientation start : coord =
  match orientation with
  | Right -> { x = dimx - start.x - 2; y = start.y }
  | Left -> { x = dimx - start.x; y = start.y }
  | Up -> { x = start.x; y = dimy - start.y - 2 }
  | Down -> { x = start.x; y = dimy - start.y }

(** [true_reflect orientation start] is the [coord] of the exit pipe
    itself. *)
let true_reflect orientation start : coord =
  match orientation with
  | Right -> { x = dimx - start.x - 1; y = start.y }
  | Left -> { x = dimx - start.x - 1; y = start.y }
  | Up -> { x = start.x; y = dimy - start.y - 1 }
  | Down -> { x = start.x; y = dimy - start.y - 1 }

(** [make_pipe_tile e c o] is the tile with coordinate [e] and tile type
    pipe with color [c] and orientation [o]. *)
let make_pipe_tile entrance color orientation =
  match color with
  | Green ->
      let end_coords = reflect_green orientation entrance in
      let pipe = { end_coords; orientation; color = Green } in
      { coords = entrance; tile_type = Pipe pipe }
  | Red -> failwith "red blah"
  | Blue -> failwith "blue blah"

let flip_orientation o =
  match o with Right -> Left | Left -> Right | Up -> Down | Down -> Up

let make_pipe_tile_pair entrance color orientation : tile list =
  let pipe = make_pipe_tile entrance color orientation in
  let pipe2 =
    make_pipe_tile
      (true_reflect orientation entrance)
      color
      (flip_orientation orientation)
  in
  [ pipe; pipe2 ]

(* let make_pipes ???? -> put it into an array of pipes make_pipe
   entrance color orientation make_pipe (entrance flipped) color
   (orientation flipped) *)

(*levels reads from json and has entrance, orientation, color

  levels has a list of "pipe info"

  levels maps each element of pipe info list into a list of tiles -
  should have all the pipe tiles (like corresponding ones)

  levels uses tile list of pipes in make_board *)

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
    | Pipe { end_coords; orientation; color } -> ()
  done

(** [make_pipes_board pipes t] adds pipe tiles [pipes] in board [t].
    Requires: the elements of pipes are tiles with tile_type pipe. *)
let rec make_pipes_board (pipes : tile list) board =
  match pipes with
  | [] -> board
  | h :: t -> (
      match h.tile_type with
      | Pipe pipe ->
          let i = index_of_coord dimx h.coords in
          board.(i) <- h;
          make_pipes_board t board
      | _ -> failwith "")

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

let alla_board entrance exit rooms pipes =
  let board = make_board entrance exit rooms in
  make_pipes_board pipes board
