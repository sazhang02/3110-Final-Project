open Board

type t = bool array

type coord = Board.coord

let dimx = 16

let dimy = 16

let collect_coin coord t =
  t.(index_of_coord dimx coord) <- true;
  t

let rec init (lst : coord list) (t' : t) =
  match lst with
  | [] -> t'
  | h :: t ->
      t'.(index_of_coord dimx h) <- true;
      t'
