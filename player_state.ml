type level_id = string

type position = int * int

type player_state = {
  current_pos : position;
  current_room : level_id;
}
