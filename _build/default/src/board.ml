open Array;
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

type square = {
  piece : piece;
  x : int;
  y : int;
}
(** dunno if I'll use this, using array indexes is probably easier*)

let space = { rank = Nothing; side = Nothing }
let pawn = { rank = Pawn; side = White }

(** board of all white pawns loll*)
let board_of_pawns = Array.make 8 (Array.make 8 pawn)

(** board of all spaces loll*)
let board_of_spaces = Array.make 8 (Array.make 8 space)

(* let set_pieces_in_space piece row_index column_index = function (Array.set
   (Array.get board_of_spaces row_index) column_index piece) row_index *)

(** function that returns the piece at the given*)
let what_piece board row_index column_index =
  Array.get (Array.get board column_index) row_index

(**not done, currently makes the row empty*)
let remove_piece board (row_index : int) (column_index : int) =
  let new_row = Array.copy (Array.get board_of_spaces column_index) in
  Array.set board column_index new_row

(** not done, currently replaces row with row of the pieces*)
let place_piece board (piece : piece) (row_index : int) (column_index : int) =
  let new_row = Array.make 8 piece in
  Array.set board column_index new_row

let move board (piece_row : int) (piece_column : int) (destination_row : int)
    (destination_column : int) =
  let piece = what_piece board piece_row piece_column in
  remove_piece board piece_row piece_column;
  place_piece board piece destination_row destination_column
