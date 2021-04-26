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
  | Entrance of orientation
  | Exit of orientation
  | Empty
  | Coin of bool

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

let get_tile_orientation tile =
  match tile.tile_type with
  | Entrance o -> o
  | Exit o -> o
  | Pipe pipe -> pipe.orientation
  | _ -> failwith ""

let room_of_coords bottom_left_start top_right_end =
  { bl_start = bottom_left_start; tr_end = top_right_end }

let get_pipe_end_of_tile tile =
  match tile.tile_type with
  | Pipe p -> p.end_coords
  | _ -> failwith "Not a pipe"

let get_pipe_color pipe = pipe.color

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
  match orientation with
  | Right -> { x = dimx - start.x - 2; y = dimy - start.y - 1 }
  | Left -> { x = dimx - start.x; y = dimy - start.y - 1 }
  | Up -> { x = dimx - start.x - 1; y = dimy - start.y - 2 }
  | Down -> { x = dimx - start.x - 1; y = dimy - start.y }

(** [rotate_blue o start] is the [coord] in front of the exit of a blue
    pipe at [start] facing [o]. *)
let rotate_blue orientation start =
  match orientation with
  | Right -> { x = start.y; y = dimx - start.x - 2 }
  | Left -> { x = start.y; y = dimx - start.x }
  | Up -> { x = start.y + 1; y = dimx - start.x - 1 }
  | Down -> { x = start.y - 1; y = dimx - start.x - 1 }

(** [pipe_end s c o] is the [coord] that a pipe at coords [s] facing [o]
    with color [c] leads to. *)
let pipe_end start color orientation =
  match color with
  | Green -> reflect_green orientation start
  | Red -> reflect_red orientation start
  | Gold -> reflect_gold orientation start
  | Blue -> rotate_blue orientation start

let make_pipe_tile entrance color orientation =
  let end_coords = pipe_end entrance color orientation in
  let pipe = { end_coords; orientation; color } in
  { coords = entrance; tile_type = Pipe pipe }

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

(* let array_tester t = for i = 0 to Array.length t - 1 do match
   t.(i).tile_type with | Entrance _ -> print_endline "entrance" | Exit
   _ -> print_endline "exit" | Empty _ -> () | Wall -> () | Pipe {
   end_coords; orientation; color } -> () done *)

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

(** [make_board en ex r] makes a board with entrance [en], exit [ex],
    and rooms [r]. *)
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
  | Coin _ -> "C"

let board_to_string (board : t) =
  let str = ref "\n" in
  for i = 0 to dimy - 1 do
    for j = 0 to dimx - 1 do
      str :=
        !str ^ "|"
        ^ tile_to_string (get_tile_c { x = j; y = dimy - 1 - i } board)
    done;
    str := !str ^ "|\n"
  done;
  !str
