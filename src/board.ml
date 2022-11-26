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
  for i = 0 to 7 do
    print_string (string_of_int (8 - i) ^ " ");
    Array.iter (fun x -> print_an_element x) board.(i);
    print_endline ""
  done;

  print_string "  ";
  print_string "a b c d e f g h ";
  print_endline "";
  ()

let print_board_black (board : piece array array) =
  for i = 0 to 7 do
    print_string (string_of_int (i + 1) ^ " ");
    Array.iter (fun x -> print_an_element x) board.(i);
    print_endline ""
  done;

  print_string "  ";
  print_string "a b c d e f g h ";
  print_endline "";

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

(** [general_moves_knight r c] is an array of coordinates of general moves by a
    knight at row r and column c*)
let general_moves_knight r c =
  [|
    (r - 2, c + 1);
    (r - 1, c + 2);
    (r + 1, c + 2);
    (r + 2, c + 1);
    (r + 2, c - 1);
    (r + 1, c - 2);
    (r - 1, c - 2);
    (r - 2, c - 1);
  |]

(** [general_moves_bishop r c] is an array of coordinates of general moves by a
    bishop at row r and column c*)
let old_general_moves_bishop r c =
  [|
    (r + 1, c + 1);
    (r + 2, c + 2);
    (r + 3, c + 3);
    (r + 4, c + 4);
    (r + 5, c + 5);
    (r + 6, c + 6);
    (r + 7, c + 7);
    (r - 1, c - 1);
    (r - 2, c - 2);
    (r - 3, c - 3);
    (r - 4, c - 4);
    (r - 5, c - 5);
    (r - 6, c - 6);
    (r - 7, c - 7);
    (r + 1, c - 1);
    (r + 2, c - 2);
    (r + 3, c - 3);
    (r + 4, c - 4);
    (r + 5, c - 5);
    (r + 6, c - 6);
    (r + 7, c - 7);
    (r - 1, c + 1);
    (r - 2, c + 2);
    (r - 3, c + 3);
    (r - 4, c + 4);
    (r - 5, c + 5);
    (r - 6, c + 6);
    (r - 7, c + 7);
  |]

let general_moves_bishop board r c =
  let new_inside_arr = ref [||] in
  let color = (what_piece board r c).side in
  let i = ref 1 in
  let continue = ref true in
  while r + !i < 8 && c + !i < 8 && !continue do
    match side_piece (what_piece board (r + !i) (c + !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + !i, c + !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + !i, c + !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r + !i, c + !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while r - !i >= 0 && c - !i >= 0 && !continue do
    match side_piece (what_piece board (r - !i) (c - !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - !i, c - !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - !i, c - !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r - !i, c - !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while r + !i < 8 && c - !i >= 0 && !continue do
    match side_piece (what_piece board (r + !i) (c - !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + !i, c - !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + !i, c - !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r + !i, c - !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while r - !i >= 0 && c + !i < 8 && !continue do
    match side_piece (what_piece board (r - !i) (c + !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - !i, c + !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - !i, c + !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r - !i, c + !i));
        i := !i + 1
  done;
  !new_inside_arr

(** [general_moves_rook r c] is an array of coordinates of general moves by a
    rook at row r and column c*)
let old_general_moves_rook r c =
  [|
    (r + 1, c);
    (r + 2, c);
    (r + 3, c);
    (r + 4, c);
    (r + 5, c);
    (r + 6, c);
    (r + 7, c);
    (r - 1, c);
    (r - 2, c);
    (r - 3, c);
    (r - 4, c);
    (r - 5, c);
    (r - 6, c);
    (r - 7, c);
    (r, c + 1);
    (r, c + 2);
    (r, c + 3);
    (r, c + 4);
    (r, c + 5);
    (r, c + 6);
    (r, c + 7);
    (r, c - 1);
    (r, c - 2);
    (r, c - 3);
    (r, c - 4);
    (r, c - 5);
    (r, c - 6);
    (r, c - 7);
  |]

let general_moves_rook board r c =
  let new_inside_arr = ref [||] in
  let color = (what_piece board r c).side in
  let i = ref 1 in
  let continue = ref true in
  while c + !i < 8 && !continue do
    match side_piece (what_piece board r (c + !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r, c + !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r, c + !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r, c + !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while c - !i >= 0 && !continue do
    match side_piece (what_piece board r (c - !i)) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r, c - !i));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r, c - !i));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r, c - !i));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while r + !i < 8 && !continue do
    match side_piece (what_piece board (r + !i) c) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + !i, c));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + !i, c));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r + !i, c));
        i := !i + 1
  done;
  let i = ref 1 in
  let continue = ref true in
  while r - !i >= 0 && !continue do
    match side_piece (what_piece board (r - !i) c) with
    | White ->
        if color = White then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - !i, c));
        continue := false
    | Black ->
        if color = Black then continue := false
        else
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - !i, c));
        continue := false
    | Nothing ->
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (r - !i, c));
        i := !i + 1
  done;
  !new_inside_arr

(** [general_moves_pawn r c] is an array of coordinates of general moves by a
    pawn at row r and column c*)
let old_general_moves_pawn r c =
  [| (r, c + 1); (r, c + 2); (r - 1, c + 1); (r + 1, c + 1) |]

let general_moves_pawn_white board new_inside_arr r c =
  if side_piece (what_piece board (r - 1) c) = Nothing then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 1, c));
  if
    r = 2
    && side_piece (what_piece board (r - 1) c) = Nothing
    && side_piece (what_piece board (r - 2) c) = Nothing
  then new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 2, c));
  if r <> 2 then (
    let right =
      match side_piece (what_piece board (r - 1) (c - 1)) with
      | White -> ()
      | Black ->
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - 1, c - 1))
      | Nothing -> ()
    in
    let left =
      match side_piece (what_piece board (r - 1) (c + 1)) with
      | White -> ()
      | Black ->
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r - 1, c + 1))
      | Nothing -> ()
    in
    right;
    left)

let general_moves_pawn_black board new_inside_arr r c =
  if side_piece (what_piece board (r + 1) c) = Nothing then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 1, c));
  if
    r = 7
    && side_piece (what_piece board (r + 1) c) = Nothing
    && side_piece (what_piece board (r + 2) c) = Nothing
  then new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 2, c));
  if r <> 7 then (
    let right =
      match side_piece (what_piece board (r + 1) (c + 1)) with
      | White ->
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + 1, c + 1))
      | Black -> ()
      | Nothing -> ()
    in
    let left =
      match side_piece (what_piece board (r + 1) (c - 1)) with
      | White ->
          new_inside_arr :=
            Array.append !new_inside_arr (Array.make 1 (r + 1, c - 1))
      | Black -> ()
      | Nothing -> ()
    in
    right;
    left)

let en_passant board new_inside_arr r c =
  let color = side_piece (what_piece board r c) in
  let right_piece = what_piece board r (c + 1) in
  let left_piece = what_piece board r (c - 1) in
  if (r <= 4 && color = Black) || (r >= 5 && color = White) then
    if side_piece right_piece <> color && enPassant_piece right_piece = true
    then
      new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r, c + 1));
  if side_piece left_piece <> color && enPassant_piece left_piece = true then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r, c - 1));
  ()

let general_moves_pawn board r c =
  let new_inside_arr = ref [||] in
  let color = side_piece (what_piece board r c) in
  if color = White then general_moves_pawn_white board new_inside_arr r c
  else general_moves_pawn_black board new_inside_arr r c;
  !new_inside_arr

(** [general_moves_king r c] is an array of coordinates of general moves by a
    king at row r and column c *)
let old_general_moves_king r c =
  [|
    (r, c + 1);
    (r, c - 1);
    (r + 1, c);
    (r - 1, c);
    (r + 1, c + 1);
    (r - 1, c - 1);
    (r + 1, c - 1);
    (r - 1, c + 1);
    (r - 2, c);
    (r + 2, c);
  |]

let helper_king board color r c =
  match side_piece (what_piece board r c) with
  | Nothing -> true
  | White -> if color = White then false else true
  | Black -> if color = Black then false else true

(** [general_moves_king r c] is an array of coordinates of general moves by a
    king at row r and column c *)
let general_moves_king board r c =
  let new_inside_arr = ref [||] in
  let color = side_piece (what_piece board r c) in
  if helper_king board color r (c + 1) then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r, c + 1))
  else ();
  if helper_king board color r (c - 1) then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r, c - 1))
  else ();
  if helper_king board color (r + 1) c then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 1, c))
  else ();
  if helper_king board color (r - 1) c then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 1, c))
  else ();
  if helper_king board color (r + 1) (c + 1) then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 1, c + 1))
  else ();
  if helper_king board color (r - 1) (c - 1) then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 1, c - 1))
  else ();
  if helper_king board color (r + 1) (c - 1) then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 1, c - 1))
  else ();
  if helper_king board color (r - 1) (c + 1) then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 1, c + 1))
  else ();
  !new_inside_arr

(** [legal_moves_knight r c] is an array of coordinates of legal moves by a
    knight at row r and column c *)
let legal_moves_knight (board : piece array array) r c =
  general_moves_knight r c |> moves_except_outside

(** [legal_moves_bishop r c] is an array of coordinates of legal moves by a
    bishop at row r and column c *)
let legal_moves_bishop board r c =
  general_moves_bishop board r c |> moves_except_outside

(** [legal_moves_rook r c] is an array of coordinates of legal moves by a rook
    at row r and column c *)
let legal_moves_rook board r c =
  general_moves_rook board r c |> moves_except_outside

(** [legal_moves_pawn r c] is an array of coordinates of legal moves by a pawn
    at row r and column c *)
let legal_moves_pawn (board : piece array array) r c =
  general_moves_pawn board r c |> moves_except_outside

(** [legal_moves_queen r c] is an array of coordinates of legal moves by a queen
    at row r and column c *)
let legal_moves_queen (board : piece array array) r c =
  let total_length_for_bishop_array =
    Array.length (general_moves_bishop board r c)
  in
  let total_length_for_rook_array =
    Array.length (general_moves_rook board r c)
  in
  let array =
    Array.make
      (total_length_for_bishop_array + total_length_for_rook_array)
      (r, c)
  in
  for i = 0 to total_length_for_bishop_array do
    array.(i) <- (general_moves_bishop board r c).(i)
  done;
  for j = 0 to total_length_for_rook_array do
    array.(j + total_length_for_bishop_array + 1) <-
      (general_moves_rook board r c).(j)
  done;
  array |> moves_except_outside

(** [legal_moves_king r c] is an array of coordinates of legal moves by a king
    at row r and column c *)
let legal_moves_king (board : piece array array) r c =
  general_moves_king board r c |> moves_except_outside

(** [is_under_attack board r c] is true if the piece at row r and column c is
    under attack in board [board] meaning it is in one of the legal moves for an
    opposing piece*)
let is_under_attack board r c =
  let side_of_piece_being_checked = side_piece (what_piece board r c) in
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
               && Array.mem (r, c) (legal_moves_pawn board i j)
      | Bishop, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (r, c) (legal_moves_bishop board i j)
      | Knight, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (r, c) (legal_moves_knight board i j)
      | Rook, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (r, c) (legal_moves_rook board i j)
      | Queen, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (r, c) (legal_moves_queen board i j)
      | King, s ->
          acc :=
            !acc
            || s <> side_of_piece_being_checked
               && Array.mem (r, c) (legal_moves_king board i j)
    done
  done;
  !acc

(*accumulator*)
(*for loop over each square of board: with access to indices *)
(*pattern-match each square: 1) Nothing -> (), 2-7) if (x,y) is in
  legal_move_<piece> <indices> then true else () *)
(*return accumulator*)

let find_king board side =
  let value = ref (-1, -1) in
  for i = 0 to 7 do
    for j = 0 to 7 do
      if
        side = (what_piece board i j).side && (what_piece board i j).rank = King
      then value := (i, j)
      else ()
    done
  done;
  !value

let is_king_in_check board (side : side) (piece_row : int) (piece_column : int)
    =
  let king_coord = find_king board side in
  is_under_attack board (fst king_coord) (snd king_coord)

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

let castle board (king_row : int) (king_col : int) (rook_row : int)
    (rook_col : int) =
  (* false if the color of king and rook are different from color of the current
     turn -> look up current_turn field in state.ml*)

  (* false if piece in given coordinates are not king and then rook*)
  let maybe_king = what_piece board king_row king_col in
  let maybe_rook = what_piece board rook_row rook_col in
  if maybe_king.rank <> King || maybe_rook.rank <> Rook then false
    (* false if king and rook already moved*)
  else if hasMoved_piece maybe_king || hasMoved_piece maybe_rook then false
    (*false if there are any pieces between king and rook*)
  else
    let any_piece_btwn = ref false in
    for i = Int.min king_col rook_col + 1 to Int.max king_col rook_col - 1 do
      if (what_piece board king_row i).rank <> Nothing then
        any_piece_btwn := true
      else ()
    done;
    if !any_piece_btwn then false
      (*false if king is in check or king passes through check*)
    else if Int.max king_col rook_col = king_col then (
      remove_piece board king_row king_col;
      place_piece board maybe_king king_row (king_col - 2);
      remove_piece board rook_row rook_col;
      place_piece board maybe_rook rook_row (rook_col + 3);
      true)
    else if Int.max king_col rook_col = rook_col then (
      remove_piece board king_row king_col;
      place_piece board maybe_king king_row (king_col + 2);
      remove_piece board rook_row rook_col;
      place_piece board maybe_rook rook_row (rook_col - 2);
      true)
    else false
