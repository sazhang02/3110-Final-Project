open Graphics

let window_size = " 1000x700"

let window_title = "This is the window title"

type coords = {x : int; y: int}

(** [current_image loc] is the image at [loc]. *)
let current_image loc = get_image loc.x loc.y 5 5

(** [draw_at_coords box loc] draws the image at the coordinates [loc]. *)
let draw_at_coords box loc = draw_image box loc.x loc.y 

(** [handle_input box pic loc new_loc] removes the previous image 
and draws a new one. *)
let handle_input box pic loc new_loc = 
  draw_image pic loc.x loc.y;
  draw_at_coords box new_loc

(** [get_input loc box prev_pic] processes keyboard inputs where w a s d
moves the player up left down right respectively. Pressing q quits the game
and anything else re-prompts the user for inputs. *)
let rec get_input loc box prev_pic = match read_key () with
  | 'w' -> 
    let new_loc = {loc with y = loc.y + 5} in 
    let current_pic = current_image new_loc in
    handle_input box prev_pic loc new_loc;
    get_input new_loc box current_pic
  | 's' -> 
    let new_loc = {loc with y = loc.y - 5} in 
    let current_pic = current_image new_loc in
    handle_input box prev_pic loc new_loc;
    get_input new_loc box current_pic
  | 'a' -> 
    let new_loc = {loc with x = loc.x - 5} in 
    let current_pic = current_image new_loc in
    handle_input box prev_pic loc new_loc;
    get_input new_loc box current_pic
  | 'd' -> 
    let new_loc = {loc with x = loc.x + 5} in 
    let current_pic = current_image new_loc in
    handle_input box prev_pic loc new_loc;
    get_input new_loc box current_pic
  | 'q' -> close_graph ()
  | _ -> get_input loc box prev_pic

(** [window] creates the gui for the game*)
let window =
  open_graph window_size;
  set_window_title window_title;
  let box = create_image 5 5 in 
  let prev_pic = current_image {x = 500; y = 500}  in
  draw_image box 500 500;
  get_input {x = 500; y = 500} box prev_pic


(* 
(*My failed attempt at getting ocamlimages to work*)

open Images
open Png

let () = Graphics.open_graph "";;

let img = load "camel.png" [];;
let g = Graphic_image.of_image img;;

Graphics.draw_image g 0 0;;

Unix.sleep 10;; *)