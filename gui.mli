(** Representation of gui data *)

(** The type representing the position of a coordinates on the GUI
    window *)
type coords

type scaling =
  | Small
  | Medium
  | Large

(** [get_x pos] is the x coordinate. *)
val get_x : coords -> int

(** [get_y pos] is the y coordinate. *)
val get_y : coords -> int

(** [make_gui_coord x y] is a gui coord with x-coord [x] and y-coord
    [y]. *)
val make_gui_coord : int -> int -> coords

(** [get_window_size scale_factor] is width and height of the window.
    Ex: (1000, 800) for a window of width 1000 and height 800. *)
val get_window_size : scaling -> int * int

(** [window_size] is the dimensions of the window. Required: If the
    string is non-empty, it must start with a space and then follow the
    format widthxheight. *)
val window_size : string

(** [window_title] is name of the window. *)
val window_title : string

val tile_width : scaling -> int

val tile_height : scaling -> int

(** [player_image_gc scaling] is player.png as a Graphics.image with
    size [scaling]. *)
val player_image_gc : scaling -> Graphics.image

(** [floor_image_gc scaling] is floor.png as a Graphics.image with size
    [scaling]. *)
val floor_image_gc : scaling -> Graphics.image

(** [coin_image_gc scaling] is coin.png as a Graphics.image with size
    [scaling]. *)
val coin_image_gc : scaling -> Graphics.image

(** [bckg_image_gc scaling] is background.png as a Graphics.image with
    size [scaling]. *)
val bckg_image_gc : scaling -> Graphics.image

(** [boss_image_gc scaling] is boss.png as a Graphics.image with size
    [scaling]. *)
val boss_image_gc : scaling -> Graphics.image

(** [load_png png_name] is the Images.t equivalent of the png file with
    name [png_name]. Required: [png_name] is the name of the directory
    reference. For example, if png.png is in the same directory, a valid
    [png_name] is "png.png". If png.pngis in folder "images" which is in
    the same directory, a valid [png_name] is "images/png.png". *)
val load_png : string -> Images.t

(** [img_width img] is width of [img] in pixels. *)
val img_width : Images.t -> int

(** [img_height img] is height of [img] in pixels. *)
val img_height : Images.t -> int

(** [draw_at_coords img loc] draws [img] at [loc] on the GUI window. *)
val draw_at_coords : Graphics.image -> coords -> unit

(** [check_movement old_loc new_loc] is True if [old_loc] is equal to
    [new_loc], otherwise it is False. *)
val check_movement : coords -> coords -> bool

(** [update_player new_img old_img new_loc old_loc] replaces the image
    at [old_loc] with [old_img] and [new_loc] with [new_img]. *)
val update_player :
  Graphics.image -> Graphics.image -> coords -> coords -> unit

(** [board_to_gui] is the Gui.coords equivalent of Board.coord. *)
val board_to_gui : scaling -> Board.coord -> coords

(* * [draw_board] draws the board. val draw_board : unit *)
val draw_board : Board.t -> scaling -> unit

(* * [display_coins p zoom] displays the coin count. *)
val display_coins : Player_state.p -> scaling -> unit

(* * [decrease_zoom player current_image zoom board] decreases the zoom
   size of the window. *)
val decrease_zoom :
  Player_state.p * Boss_state.b option ->
  Graphics.image * Graphics.image option ->
  scaling ->
  Board.t ->
  scaling * (Graphics.image * Graphics.image option)

(* * [increase_zoom player current_image zoom board] increases the zoom
   size of the window. *)
val increase_zoom :
  Player_state.p * Boss_state.b option ->
  Graphics.image * Graphics.image option ->
  scaling ->
  Board.t ->
  scaling * (Graphics.image * Graphics.image option)

(** [set_up_level p board zoom] sets up the level with its corresponding
    [board]. The board for the level is drawn and the player is drawn
    according to the information in [p]. *)
val set_up_level : Player_state.p -> Board.t -> scaling -> unit

(** [get_image loc zoom] is the image at [loc] with size determined by
    [zoom]. *)
val get_image : coords -> scaling -> Graphics.image

(** [update_player_boss new_imgs old_imgs new_locs old_locs] replaces
    the image at [old_loc] with [old_img] and [new_loc] with [new_img]
    for the player and boss respectively. Note that the tuples are
    stored with player related information first then boss related
    information second. *)
val update_player_boss :
  Graphics.image * Graphics.image ->
  Graphics.image * Graphics.image ->
  coords * coords ->
  coords * coords ->
  unit
