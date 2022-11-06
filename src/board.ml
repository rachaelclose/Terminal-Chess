open Printf
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

type board = piece array array

(** dunno if I'll use this, using array indexes is probably easier*)

let space = { rank = Nothing; side = Nothing }
let pawn = { rank = Pawn; side = White }

(** board of all white pawns loll*)
let board_of_pawns = Array.make 8 (Array.make 8 pawn)

(** board of all spaces loll*)
let board_of_spaces = Array.make 8 (Array.make 8 space)

(** board of all pieces*)
let board_of_pieces = Array.make_matrix 8 8 pawn

let board_of_game =
  let black_pawn = { pawn with side = Black } in
  board_of_pieces.(0).(0) <- { black_pawn with rank = Rook };
  board_of_pieces.(0).(1) <- { black_pawn with rank = Knight };
  board_of_pieces.(0).(2) <- { black_pawn with rank = Bishop };
  board_of_pieces.(0).(3) <- { black_pawn with rank = King };
  board_of_pieces.(0).(4) <- { black_pawn with rank = Queen };
  board_of_pieces.(0).(5) <- { black_pawn with rank = Bishop };
  board_of_pieces.(0).(6) <- { black_pawn with rank = Knight };
  board_of_pieces.(0).(7) <- { black_pawn with rank = Rook };
  for i = 0 to 7 do
    board_of_pieces.(1).(i) <- black_pawn
  done;
  for i = 2 to 5 do
    for j = 0 to 7 do
      board_of_pieces.(i).(j) <- space
    done
  done;
  for i = 0 to 7 do
    board_of_pieces.(6).(i) <- pawn
  done;
  board_of_pieces.(7).(0) <- { pawn with rank = Rook };
  board_of_pieces.(7).(1) <- { pawn with rank = Knight };
  board_of_pieces.(7).(2) <- { pawn with rank = Bishop };
  board_of_pieces.(7).(3) <- { pawn with rank = King };
  board_of_pieces.(7).(4) <- { pawn with rank = Queen };
  board_of_pieces.(7).(5) <- { pawn with rank = Bishop };
  board_of_pieces.(7).(6) <- { pawn with rank = Knight };
  board_of_pieces.(7).(7) <- { pawn with rank = Rook }

(** allow access of piece's rank outside this file*)
let rank_piece piece = piece.rank

(** allow access of piece's side outside this file*)
let side_piece piece = piece.side

(* let set_pieces_in_space piece row_index column_index = function (Array.set
   (Array.get board_of_spaces row_index) column_index piece) row_index *)

(** function that returns the piece at the given*)
let what_piece board row_index column_index =
  Array.get (Array.get board row_index) column_index

let what_piece_mvp board index = Array.get board index

(**done? Should remove the piece at the specified index*)
let remove_piece (board : board) (row_index : int) (column_index : int) =
  let new_row = Array.copy (Array.get board row_index) in
  Array.set new_row column_index space;
  Array.set board row_index new_row

let remove_piece_mvp board (index : int) = Array.set board index space

(** not done, currently replaces row with row of the pieces*)
let place_piece (board : board) (piece : piece) (row_index : int)
    (column_index : int) =
  let new_row = Array.copy (Array.get board row_index) in
  Array.set new_row column_index piece;
  Array.set board row_index new_row

let place_piece_mvp board (piece : piece) (index : int) =
  Array.set board index piece

let move_mvp board (piece_index : int) (destination_index : int) =
  let piece = what_piece_mvp board piece_index in
  remove_piece_mvp board piece_index;
  place_piece_mvp board piece destination_index

(** match piece with the corresponding representing letter*)
let matching piece =
  let m = rank_piece piece in
  let n = side_piece piece in
  match (m, n) with
  | Pawn, White -> "♙"
  | Bishop, White -> "♗"
  | Knight, White -> "♘"
  | Rook, White -> "♖"
  | Queen, White -> "♕"
  | King, White -> "♔"
  | Pawn, Black -> "♟"
  | Bishop, Black -> "♝"
  | Knight, Black -> "♞"
  | Rook, Black -> "♜"
  | Queen, Black -> "♛"
  | King, Black -> "♚"
  | _ -> "_"

(** print a single piece's letter*)
let print_an_element piece = print_string (matching piece ^ " ")

(** print the current board*)
let print_board (board : piece array array) =
  board
  |> Array.iter (fun x ->
         Array.iter print_an_element x;
         print_endline "")

(******************************************************************************)

(** master function for chess move*)
let move board (piece_row : int) (piece_column : int) (destination_row : int)
    (destination_column : int) =
  let piece = what_piece board piece_row piece_column in
  remove_piece board piece_row piece_column;
  place_piece board piece destination_row destination_column

(** [moves_except_outside arr] is a new array without moves outside of the board
    from array [arr]*)
let moves_except_outside arr =
  let new_inside_arr = ref [||] in
  for i = 0 to Array.length arr do
    if fst arr.(i) > 7 || fst arr.(i) < 0 || snd arr.(i) > 7 || snd arr.(i) < 0
    then ()
    else new_inside_arr := Array.append !new_inside_arr (Array.make 1 arr.(i))
  done;
  !new_inside_arr

(** [general_moves_knight x y] is an array of coordinates of general moves by a
    knight in coordinate (x,y)*)
let general_moves_knight x y =
  [|
    (x - 2, y + 1);
    (x - 1, y + 2);
    (x + 1, y + 2);
    (x + 2, y + 1);
    (x + 2, y - 1);
    (x + 1, y - 2);
    (x - 1, y - 2);
    (x - 2, y - 1);
  |]

(** [legal_moves_knight x y] is an array of coordinates of legal moves by a
    knight in coordinate (x,y) *)
let legal_moves_knight x y = general_moves_knight x y |> moves_except_outside
