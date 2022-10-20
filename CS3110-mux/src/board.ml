open Array
(** creates a chessboard*)

type rank =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King
  | Nothing

type side =
  | White
  | Black
  | Nothing

type piece = {
  rank : rank;
  side : side;
}

(** dunno if I'll use this, using array indexes is probably easier*)
type square = {
  piece : piece;
  x : int;
  y : int;
}


let space = {rank = Nothing; side = Nothing}
let pawn = {rank = Pawn; side = White}

(** board of all white pawns loll*)
let board_of_pawns = Array.make 8 (Array.make 8 pawn)

(** board of all spaces loll*)
let board_of_spaces = Array.make 8 (Array.make 8 space)

(* let set_pieces_in_space piece row_index column_index = function
  (Array.set (Array.get board_of_spaces row_index) column_index piece) row_index  *)

(** function that returns the piece at the given*)
let what_piece row_index column_index = Array.get (Array.get board_of_spaces column_index) row_index

(**not done*)
let remove_piece row_index column_index = Array.set board_of_pawns column_index (Array.get board_of_pawns row_index)

let place_piece piece x y = function

let move a b x y = 
  remove_piece a b;
  let piece = (what_piece a b) in
    remove_piece a b;
    place_piece piece x y;
