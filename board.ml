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
  | Gold

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
  | Entrance of orientation
  | Exit of orientation
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

let get_tile_orientation e =
  match e.tile_type with
  | Entrance o -> o
  | Exit o -> o
  | Pipe pipe -> pipe.orientation
  | _ -> failwith ""

let room_of_coords bottom_left_start top_right_end =
  { bl_start = bottom_left_start; tr_end = top_right_end }

let get_pipe_end pipe = pipe.end_coords

let get_pipe_end_of_tile tile =
  match tile.tile_type with
  | Pipe p -> get_pipe_end p
  | _ -> failwith "Not a pipe"

let get_pipe_color pipe = pipe.color

(* let get_pipe_orientation pipe = pipe.orientation *)

(** [true_reflect orientation start] is the [coord] of the exit pipe
    itself. *)
let true_reflect orientation start : coord =
  match orientation with
  | Right | Left -> { x = dimx - start.x - 1; y = start.y }
  | Up | Down -> { x = start.x; y = dimy - start.y - 1 }

(** [reflect_green orientation start] is the [coord] in front of the
    exit of a green pipe at [start] facing [orientation]. *)
let reflect_green orientation start : coord =
  match orientation with
  | Right -> { x = dimx - start.x - 2; y = start.y }
  | Left -> { x = dimx - start.x; y = start.y }
  | Up -> { x = start.x; y = dimy - start.y - 2 }
  | Down -> { x = start.x; y = dimy - start.y }

(** [reflect_red o start] is the [coord] in front of the exit of a red
    pipe at [start] facing [o]. *)
let reflect_red orientation start : coord =
  match orientation with
  | Right -> { x = start.x + 1; y = dimy - start.y - 1 }
  | Left -> { x = start.x - 1; y = dimy - start.y - 1 }
  | Up -> { x = dimx - start.x - 1; y = start.y + 1 }
  | Down -> { x = dimx - start.x - 1; y = start.y - 1 }

(** [reflect_gold o start] is the [coord] in front of the exit of a gold
    pipe at [start] facing [o]. *)
let reflect_gold orientation start =
  true_reflect orientation (reflect_red orientation start)

let make_pipe_tile entrance color orientation =
  match color with
  | Green ->
      let end_coords = reflect_green orientation entrance in
      let pipe = { end_coords; orientation; color = Green } in
      { coords = entrance; tile_type = Pipe pipe }
  | Red ->
      let end_coords = reflect_red orientation entrance in
      let pipe = { end_coords; orientation; color = Red } in
      { coords = entrance; tile_type = Pipe pipe }
  | Gold ->
      let end_coords = reflect_gold orientation entrance in
      let pipe = { end_coords; orientation; color = Gold } in
      { coords = entrance; tile_type = Pipe pipe }

let flip_orientation o =
  match o with Right -> Left | Left -> Right | Up -> Down | Down -> Up

let red_pipe_pair entrance orientation : tile list =
  let pipe1 = make_pipe_tile entrance Red orientation in
  let pipe2 =
    let en =
      match orientation with
      | Right | Left -> true_reflect Up entrance
      | Up | Down -> true_reflect Right entrance
    in
    make_pipe_tile en Red orientation
  in
  [ pipe1; pipe2 ]

let green_pipe_pair entrance orientation : tile list =
  let pipe1 = make_pipe_tile entrance Green orientation in
  let pipe2 =
    make_pipe_tile
      (true_reflect orientation entrance)
      Green
      (flip_orientation orientation)
  in
  [ pipe1; pipe2 ]

let gold_pipe_pair entrance orientation =
  let pipe1 = make_pipe_tile entrance Gold orientation in
  let pipe2 =
    make_pipe_tile
      (reflect_green
         (flip_orientation orientation)
         (reflect_red orientation entrance))
      Gold
      (flip_orientation orientation)
  in
  [ pipe1; pipe2 ]

let make_pipe_tile_pair entrance color orientation : tile list =
  match color with
  | Green -> green_pipe_pair entrance orientation
  | Red -> red_pipe_pair entrance orientation
  | Gold -> gold_pipe_pair entrance orientation

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
    | Entrance _ -> print_endline "entrance"
    | Exit _ -> print_endline "exit"
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
      | _ -> failwith "" )

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

let tile_to_string tile =
  match tile.tile_type with
  | Wall -> "W"
  | Pipe p -> (
      match p.orientation with
      | Right -> ">"
      | Left -> "<"
      | Up -> "^"
      | Down -> "v" )
  | Entrance _ -> "I"
  | Exit _ -> "O"
  | Empty -> " "

(* coordinates, color, tile_type *)
let print_board (board : t) =
  let str = ref "" in
  for i = dimy - 1 to 0 do
    for j = 0 to dimx - 1 do
      str :=
        !str ^ "|" ^ tile_to_string (get_tile_c { x = j; y = i } board)
    done
  done
