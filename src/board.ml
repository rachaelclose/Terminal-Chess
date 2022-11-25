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
  hasMoved : bool;
  enPassant : bool;
}

type square = {
  piece : piece;
  x : int;
  y : int;
}

type board = piece array array

(**[piece_in_tuple piece] is a tuple (rank, side) of a [piece] *)
let piece_in_tuple piece =
  let r = piece.rank in
  let s = piece.side in
  (r, s)

let space =
  { rank = Nothing; side = Nothing; hasMoved = false; enPassant = false }

let pawn = { rank = Pawn; side = White; hasMoved = false; enPassant = false }
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

(** allow access of piece's hasMoved outside this file*)
let hasMoved_piece piece = piece.hasMoved

(** allow access of piece's enPassant outside this file*)
let enPassant_piece piece = piece.enPassant

(* let set_pieces_in_space piece row_index column_index = function (Array.set
   (Array.get board_of_spaces row_index) column_index piece) row_index *)

let what_piece board row_index column_index =
  Array.get (Array.get board row_index) column_index

let remove_piece (board : board) (row_index : int) (column_index : int) =
  let new_row = Array.copy (Array.get board row_index) in
  Array.set new_row column_index space;
  Array.set board row_index new_row

let place_piece (board : board) (piece : piece) (row_index : int)
    (column_index : int) =
  let new_row = Array.copy (Array.get board row_index) in
  Array.set new_row column_index piece;
  Array.set board row_index new_row

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
  print_string "  ";
  print_string "a b c d e f g h ";
  print_endline "";

  for i = 0 to 7 do
    print_string (string_of_int (8 - i) ^ " ");
    Array.iter (fun x -> print_an_element x) board.(i);
    print_endline ""
  done;
  ()

let print_board_black (board : piece array array) =
  print_string "  ";
  print_string "a b c d e f g h ";
  print_endline "";

  for i = 0 to 7 do
    print_string (string_of_int (i + 1) ^ " ");
    Array.iter (fun x -> print_an_element x) board.(i);
    print_endline ""
  done;
  ()

(******************************************************************************)

(** [moves_except_outside arr] is a new array without moves outside of the board
    from array [arr]*)
let moves_except_outside arr =
  let new_inside_arr = ref [||] in
  for i = 0 to Array.length arr - 1 do
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

(** [general_moves_bishop x y] is an array of coordinates of general moves by a
    bishop in coordinate (x,y)*)
let old_general_moves_bishop x y =
  [|
    (x + 1, y + 1);
    (x + 2, y + 2);
    (x + 3, y + 3);
    (x + 4, y + 4);
    (x + 5, y + 5);
    (x + 6, y + 6);
    (x + 7, y + 7);
    (x - 1, y - 1);
    (x - 2, y - 2);
    (x - 3, y - 3);
    (x - 4, y - 4);
    (x - 5, y - 5);
    (x - 6, y - 6);
    (x - 7, y - 7);
    (x + 1, y - 1);
    (x + 2, y - 2);
    (x + 3, y - 3);
    (x + 4, y - 4);
    (x + 5, y - 5);
    (x + 6, y - 6);
    (x + 7, y - 7);
    (x - 1, y + 1);
    (x - 2, y + 2);
    (x - 3, y + 3);
    (x - 4, y + 4);
    (x - 5, y + 5);
    (x - 6, y + 6);
    (x - 7, y + 7);
  |]

let general_moves_bishop board x y =
  let new_inside_arr = ref [||] in
  let color = (what_piece board x y).side in
  let i = ref 1 in
  let continue = ref true in
  while x + !i < 8 && y + !i < 8 && !continue do
    match side_piece (what_piece board (x + !i) (y + !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x + !i, y + !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x + !i, y + !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x + !i, y + !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while x - !i >= 0 && y - !i >= 0 && !continue do
    match side_piece (what_piece board (x - !i) (y - !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x - !i, y - !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x - !i, y - !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x - !i, y - !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while x + !i < 8 && y - !i >= 0 && !continue do
    match side_piece (what_piece board (x + !i) (y - !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x + !i, y - !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x + !i, y - !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x + !i, y - !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while x - !i >= 0 && y + !i < 8 && !continue do
    match side_piece (what_piece board (x - !i) (y + !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x - !i, y + !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x - !i, y + !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x - !i, y + !i));
        i := !i + 1
  done;
  !new_inside_arr

(** [general_moves_rook x y] is an array of coordinates of general moves by a
    rook in coordinate (x,y)*)
let old_general_moves_rook x y =
  [|
    (x + 1, y);
    (x + 2, y);
    (x + 3, y);
    (x + 4, y);
    (x + 5, y);
    (x + 6, y);
    (x + 7, y);
    (x - 1, y);
    (x - 2, y);
    (x - 3, y);
    (x - 4, y);
    (x - 5, y);
    (x - 6, y);
    (x - 7, y);
    (x, y + 1);
    (x, y + 2);
    (x, y + 3);
    (x, y + 4);
    (x, y + 5);
    (x, y + 6);
    (x, y + 7);
    (x, y - 1);
    (x, y - 2);
    (x, y - 3);
    (x, y - 4);
    (x, y - 5);
    (x, y - 6);
    (x, y - 7);
  |]

let general_moves_rook board x y =
  let new_inside_arr = ref [||] in
  let color = (what_piece board x y).side in
  let i = ref 1 in
  let continue = ref true in
  while y + !i < 8 && !continue do
    match side_piece (what_piece board x (y + !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x, y + !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x, y + !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x + !i, y + !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while y - !i >= 0 && !continue do
    match side_piece (what_piece board x (y - !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x, y - !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x, y - !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x - !i, y - !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while x + !i < 8 && !continue do
    match side_piece (what_piece board (x + !i) y) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x + !i, y));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x + !i, y));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x + !i, y - !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while x - !i >= 0 && !continue do
    match side_piece (what_piece board (x - !i) y) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x - !i, y));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (x - !i, y));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (x - !i, y + !i));
        i := !i + 1
  done;
  !new_inside_arr

(** [general_moves_pawn x y] is an array of coordinates of general moves by a
    pawn in coordinate (x,y)*)
let old_general_moves_pawn x y =
  [| (x, y + 1); (x, y + 2); (x - 1, y + 1); (x + 1, y + 1) |]

let general_moves_pawn board x y =
  let new_inside_arr = ref [||] in
  let color = (what_piece board x y).side in
  (*let hasMoved = hasMoved_piece (what_piece board x y) in*)
  if side_piece (what_piece board (x + 1) y) = Nothing && color = Black then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (x + 1, y));
  if side_piece (what_piece board (x - 1) y) = Nothing && color = White then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (x - 1, y));
  if
    x = 1 && color = Black
    && side_piece (what_piece board (x + 1) y) = Nothing
    && side_piece (what_piece board (x + 2) y) = Nothing
  then new_inside_arr := Array.append !new_inside_arr (Array.make 1 (x + 2, y));
  if
    (x = 6 && color = White)
    && side_piece (what_piece board (x - 1) y) = Nothing
    && side_piece (what_piece board (x - 2) y) = Nothing
  then new_inside_arr := Array.append !new_inside_arr (Array.make 1 (x - 2, y));
  let result = !new_inside_arr in
  result
(*let right = match side_piece (what_piece board (x + 1) (y + 1)) with | White
  -> if color <> White then new_inside_arr := Array.append !new_inside_arr
  (Array.make 1 (x + 1, y + 1)) | Black -> if color <> Black then new_inside_arr
  := Array.append !new_inside_arr (Array.make 1 (x + 1, y + 1)) | Nothing -> ()
  in let left = match side_piece (what_piece board (x - 1) (y + 1)) with | White
  -> if color <> White then new_inside_arr := Array.append !new_inside_arr
  (Array.make 1 (x - 1, y + 1)) | Black -> if color <> Black then new_inside_arr
  := Array.append !new_inside_arr (Array.make 1 (x - 1, y + 1)) | Nothing -> ()
  (*add en passsant*) in*)

(** [general_moves_king x y] is an array of coordinates of general moves by a
    king in coordinate (x,y)*)
let general_moves_king x y =
  [|
    (x, y + 1);
    (x, y - 1);
    (x + 1, y);
    (x - 1, y);
    (x + 1, y + 1);
    (x - 1, y - 1);
    (x + 1, y - 1);
    (x - 1, y + 1);
    (x - 2, y);
    (x + 2, y);
  |]

(** [legal_moves_knight x y] is an array of coordinates of legal moves by a
    knight in coordinate (x,y) *)
let legal_moves_knight (board : piece array array) x y =
  general_moves_knight x y |> moves_except_outside

(** [legal_moves_bishop x y] is an array of coordinates of legal moves by a
    bishop in coordinate (x,y) *)
let legal_moves_bishop board x y =
  general_moves_bishop board x y |> moves_except_outside

(** [legal_moves_rook x y] is an array of coordinates of legal moves by a rook
    in coordinate (x,y) *)
let legal_moves_rook board x y =
  general_moves_rook board x y |> moves_except_outside

(** [legal_moves_pawn x y] is an array of coordinates of legal moves by a pawn
    in coordinate (x,y) *)
let legal_moves_pawn (board : piece array array) x y =
  general_moves_pawn board x y |> moves_except_outside

(** [legal_moves_queen x y] is an array of coordinates of legal moves by a queen
    in coordinate (x,y) *)
let legal_moves_queen (board : piece array array) x y =
  let total_length_for_bishop_array =
    Array.length (general_moves_bishop board x y)
  in
  let total_length_for_rook_array =
    Array.length (general_moves_rook board x y)
  in
  let array =
    Array.make
      (total_length_for_bishop_array + total_length_for_rook_array)
      (x, y)
  in
  for i = 0 to total_length_for_bishop_array do
    array.(i) <- (general_moves_bishop board x y).(i)
  done;
  for j = 0 to total_length_for_rook_array do
    array.(j + total_length_for_bishop_array + 1) <-
      (general_moves_rook board x y).(j)
  done;
  array |> moves_except_outside

(** [legal_moves_king x y] is an array of coordinates of legal moves by a king
    in coordinate (x,y) *)
let legal_moves_king (board : piece array array) x y =
  general_moves_king x y |> moves_except_outside

(** [is_under_attack board x y] is true if the piece in coordinate (x,y) is
    under attack in board [board] meaning it is in one of the legal moves for an
    opposing piece*)
let is_under_attack board x y =
  let side_of_piece_being_checked = side_piece (what_piece board x y) in
  let acc = ref false in
  for i = 0 to 7 do
    for j = 0 to 7 do
      match what_piece board i j |> piece_in_tuple with
      | Nothing, _ -> ()
      | _, Nothing -> ()
      | Pawn, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (x, y) (legal_moves_pawn board i j)
      | Bishop, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (x, y) (legal_moves_bishop board i j)
      | Knight, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (x, y) (legal_moves_knight board i j)
      | Rook, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (x, y) (legal_moves_rook board i j)
      | Queen, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (x, y) (legal_moves_queen board i j)
      | King, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (x, y) (legal_moves_king board i j)
    done
  done;
  !acc

(*accumulator*)
(*for loop over each square of board: with access to indices *)
(*pattern-match each square: 1) Nothing -> (), 2-7) if (x,y) is in
  legal_move_<piece> <indices> then true else () *)
(*return accumulator*)

(** let find_row_of_piece board side rank= let value = ~-1 in for i= 0 to 7 do
    for j=0 to 7 do if ((side = side_piece (what_piece board i j)) && (rank =
    what_piece board i j)) then value = i else value = ~-1 done; done; value

    let find_column_of_piece board side rank = let value = ~-1 in for i= 0 to 7
    do for j=0 to 7 do if ((side = side_piece (what_piece board i j)) && (rank =
    what_piece board i j)) then value = j else value = ~-1 done; done; value

    let is_king_in_check board (piece_row : int) (piece_column : int) = let
    move_piece_side = side_piece (what_piece board piece_row piece_column) in
    let row = find_row_of_piece board move_piece_side King in let column =
    find_column_of_piece board move_piece_side King in Array.mem (row, column)
    (legal_moves_knight piece_row piece_column) *)

(** If the king is in any of the legal moves from the other side's pieces *)

let move_piece board legal_moves (piece_row : int) (piece_column : int)
    (destination_row : int) (destination_column : int) =
  if
    Array.mem
      (destination_row, destination_column)
      (legal_moves board piece_row piece_column)
  then (
    let mov_piece = what_piece board piece_row piece_column in

    remove_piece board piece_row piece_column;
    place_piece board mov_piece destination_row destination_column;
    true)
  else false
(* ^^ Prob in this function.. Check if king is under attack?*)

(** master function for chess move*)
let move board (piece_row : int) (piece_column : int) (destination_row : int)
    (destination_column : int) =
  let piece =
    { (what_piece board piece_row piece_column) with hasMoved = true }
  in
  match rank_piece piece with
  | Knight ->
      move_piece board legal_moves_knight piece_row piece_column destination_row
        destination_column
  | Bishop ->
      move_piece board legal_moves_bishop piece_row piece_column destination_row
        destination_column
  | Rook ->
      move_piece board legal_moves_rook piece_row piece_column destination_row
        destination_column
  | King ->
      move_piece board legal_moves_king piece_row piece_column destination_row
        destination_column
  | Queen ->
      move_piece board legal_moves_queen piece_row piece_column destination_row
        destination_column
  | Pawn ->
      move_piece board legal_moves_pawn piece_row piece_column destination_row
        destination_column
  | _ ->
      move_piece board legal_moves_knight piece_row piece_column destination_row
        destination_column
