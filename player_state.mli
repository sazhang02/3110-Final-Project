(* The abstract type representing player state *)
type p

(* The type of the level identifier. Each level's [level_id] is unique. *)
type level_id = int

(* The abstract type representing player's intended move *)
type move

(** [init_state t bt] is the initial state of the game when playing in
    each level. In that state the character is currently located in the
    starting position, and they have visited only that level. *)
val init_state : Levels.t -> Board.t -> p

(** [get_current_level p] is the identifier of the level in which the
    player currently is located in state [p]. *)
val get_current_level : p -> Levels.level_id

(** [get_current_tile p] is the tile in which the player currently is
    located in state [p]. *)
val get_current_tile : p -> Board.tile

(** [get current_pos p] is the position coordinate in which the player
    currently is located in state [p]. *)
val get_current_pos : p -> Board.coord

(** [get_coins p] is the coin count for the coins that the player
    currently has in state [p]. *)
val get_coins : p -> int

(** [update m p t bt] is a state where the . *)
val update : char -> p -> Levels.t -> Board.t -> p

(** [make_player_state x y tile id coins] is the player state with
    [tile] at coordinate [(x, y)], level at [id], and [coin] number of
    coins. *)
val make_player_state :
  int -> int -> Board.tile_type -> level_id -> int -> p
