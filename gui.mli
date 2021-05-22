(** Representation of gui data *)

(** The type representing the position of a coordinate on the GUI window *)
type coords

(** [scaling] representing the zoom size of the GUI window in three
    different scales: Small, Medium, Large *)
type scaling =
  | Small
  | Medium
  | Large

(** [get_x pos] is the x coordinate with respect to the GUI coordinate
    grid. *)
val get_x : coords -> int

(** [get_y pos] is the y coordinate with respect to the GUI coordinate
    grid. *)
val get_y : coords -> int

(** [make_gui_coord x y] is a GUI coordinate with x-coordinate [x] and
    y-coordinate [y]. *)
val make_gui_coord : int -> int -> coords

(** [get_window_size scaling] is the width and height of the GUI window.
    For example, (1000, 800) is for a window of width 1000 pixels and
    height 800 pixels. *)
val get_window_size : scaling -> int * int

(** [window_size] is the dimensions of the window. Required: If the
    string is non-empty, it must start with a space and then follow the
    format widthxheight. For example, " 1100x800" *)
val window_size : string

(** [window_title] is name of the window. *)
val window_title : string

(** [player_image_gc scale] is player.png as a Graphics.image with
    scaling size [scale]. *)
val player_image_gc : scaling -> Graphics.image

(** [floor_image_gc scale] is floor.png as a Graphics.image with scaling
    size [scale]. *)
val floor_image_gc : scaling -> Graphics.image

(** [coin_image_gc scale] is coin.png as a Graphics.image with scaling
    size [scale]. *)
val coin_image_gc : scaling -> Graphics.image

(** [bckg_image_gc scale] is background.png as a Graphics.image with
    scaling size [scale]. *)
val bckg_image_gc : scaling -> Graphics.image

(** [boss_image_gc scale] is boss.png as a Graphics.image with scaling
    size [scale]. *)
val boss_image_gc : scaling -> Graphics.image

(** [title_img_gc scale] is title.png as a Graphics.image with scaling
    size [scale].*)
val title_image_gc : scaling -> Graphics.image

(** [start_img ()] is start_game.png as a Graphics.image with scaling
    size Large. *)
val start_img : unit -> Graphics.image

(** [easy_unselected_img ()] is easy_unselected.png as a Graphics.image
    with scaling size Large. *)
val easy_unselected_img : unit -> Graphics.image

(** [easy_selected_img ()] is easy_selected.png as a Graphics.image with
    scaling size Large. *)
val easy_selected_img : unit -> Graphics.image

(** [hard_unselected_img ()] is hard_unselected.png as a Graphics.image
    with scaling size Large. *)
val hard_unselected_img : unit -> Graphics.image

(** [hard_selected_img ()] is hard_selected.png as a Graphics.image with
    scaling size Large.*)
val hard_selected_img : unit -> Graphics.image

(** [load_png png_name] is the Images.t equivalent of the png file with
    name [png_name]. Required: [png_name] is the name of the directory
    reference. For example, if png.png is in the same directory, a valid
    [png_name] is "png.png". If png.png is in folder "images" which is
    in the same directory, a valid [png_name] is "images/png.png". *)
val load_png : string -> Images.t

(** [img_width img] is width of [img] in pixels. *)
val img_width : Images.t -> int

(** [img_height img] is height of [img] in pixels. *)
val img_height : Images.t -> int

(** [draw_at_coords img loc] draws [img] at [loc] on the GUI window. *)
val draw_at_coords : Graphics.image -> coords -> unit

(** [check_movement old_loc new_loc] is true if [old_loc] is equal to
    [new_loc], otherwise it is false. *)
val check_movement : coords -> coords -> bool

(** [update_player new_img old_img new_loc old_loc] replaces the image
    at [old_loc] with [old_img] and [new_loc] with [new_img]. *)
val update_player :
  Graphics.image -> Graphics.image -> coords -> coords -> unit

(** [board_to_gui] is the Gui.coords equivalent of Board.coord
    (translates coordinate under Board coordinate grid to one of the GUI
    coordinate grid). *)
val board_to_gui : scaling -> Board.coord -> coords

(** [draw_tile scale_factor tile coords tile_type] draws [tile] at
    [coords] on the board displayed on the GUI window. *)
val draw_tile :
  scaling -> Board.tile -> coords -> Board.tile_type -> unit

(** [draw_board t scale] draws the board [t] at scaling size [scale] on
    the GUI window. *)
val draw_board : Board.t -> scaling -> unit

(** [display_coins p scale] displays the coin count on the GUI window. *)
val display_coins : Player_state.p -> scaling -> unit

(**[display_damage b scale] displays the boss' remaining health on the
   GUI window. *)
val display_damage : Boss_state.b -> scaling -> unit

(** [display_steps p scale] displays the total number of steps taken on
    the GUI window. *)
val display_steps : Player_state.p -> scaling -> unit

(** [display_score steps scale] displays the final score on the GUI
    window. *)
val display_score : int -> scaling -> unit

(** [decrease_zoom pb current_imgs prev_imgs scale board] decreases the
    scale size of GUI the window. *)
val decrease_zoom :
  Player_state.p * Boss_state.b option ->
  Graphics.image * Graphics.image option ->
  Graphics.image * Graphics.image option ->
  scaling ->
  Board.t ->
  scaling
  * ( (Graphics.image * Graphics.image option)
    * (Graphics.image * Graphics.image option) )

(** [increase_zoom pb current_imgs prev_imgs scale board] increases the
    scale size of the GUI window. *)
val increase_zoom :
  Player_state.p * Boss_state.b option ->
  Graphics.image * Graphics.image option ->
  Graphics.image * Graphics.image option ->
  scaling ->
  Board.t ->
  scaling
  * ( (Graphics.image * Graphics.image option)
    * (Graphics.image * Graphics.image option) )

(** [set_up_level p board scale] sets up the level with its
    corresponding [board]. The board for the level is drawn and the
    player is drawn according to the information stored in [p]. *)
val set_up_level : Player_state.p -> Board.t -> scaling -> unit

(** [get_image loc scale] is the image at [loc] with size determined by
    scaling size [scale]. *)
val get_image : coords -> scaling -> Graphics.image

(** [draw_screen_background scale] draws the floor tile on the entire
    GUI window. *)
val draw_screen_background : scaling -> unit

(** [update_player_boss new_imgs old_imgs new_locs old_locs] replaces
    the images at [old_locs] with [old_imgs] and [new_locs] with
    [new_imgs] for the player and boss respectively. Note that the
    tuples are stored with player related information first then boss
    related information second. *)
val update_player_boss :
  Graphics.image * Graphics.image ->
  Graphics.image * Graphics.image ->
  coords * coords ->
  coords * coords ->
  unit
