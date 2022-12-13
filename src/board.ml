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
  mutable hasMoved : bool;
  mutable enPassant : bool;
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
let black_pawn = { pawn with side = Black }
let board_of_pieces = Array.make_matrix 8 8 pawn

let board_of_game =
  board_of_pieces.(0).(0) <- { black_pawn with rank = Rook };
  board_of_pieces.(0).(1) <- { black_pawn with rank = Knight };
  board_of_pieces.(0).(2) <- { black_pawn with rank = Bishop };
  board_of_pieces.(0).(3) <- { black_pawn with rank = Queen };
  board_of_pieces.(0).(4) <- { black_pawn with rank = King };
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
  board_of_pieces.(7).(3) <- { pawn with rank = Queen };
  board_of_pieces.(7).(4) <- { pawn with rank = King };
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

(** allow access to the White/Black's turn outside this file*)
let whites_turn = ref true

(* let set_pieces_in_space piece row_index column_index = function (Array.set
   (Array.get board_of_spaces row_index) column_index piece) row_index *)

let what_piece board row_index column_index =
  Array.get (Array.get board row_index) column_index

let remove_piece (board : board) (row_index : int) (column_index : int) =
  let new_row = Array.copy (Array.get board row_index) in
  Array.set new_row column_index space;
  Array.set board row_index new_row

let place_piece (board : board) (piece : piece) (row_index : int)
    (column_index : int) (moved : bool) =
  let new_row = Array.copy (Array.get board row_index) in
  Array.set new_row column_index { piece with hasMoved = moved };
  Array.set board row_index new_row

let set_en_passant (board : board) (row_index : int) (column_index : int) =
  let new_row = Array.copy (Array.get board row_index) in
  Array.set new_row column_index
    { (what_piece board row_index column_index) with enPassant = true };
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

let unmatching char =
  match char with
  | "♙" -> pawn
  | "♗" -> { pawn with rank = Bishop }
  | "♘" -> { pawn with rank = Knight }
  | "♖" -> { pawn with rank = Rook }
  | "♕" -> { pawn with rank = Queen }
  | "♟" -> black_pawn
  | "♝" -> { black_pawn with rank = Bishop }
  | "♞" -> { black_pawn with rank = Knight }
  | "♜" -> { black_pawn with rank = Rook }
  | "♛" -> { black_pawn with rank = Queen }
  | "♚" -> { black_pawn with rank = King }
  | _ -> space

let match_side side =
  match side with
  | White -> "white"
  | Black -> "black"
  | Nothing -> "none"

(** print a single piece's letter*)
let print_an_element piece = print_string (matching piece ^ " ")

(** print the current board*)
let print_board_white (board : piece array array) =
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
  for i = 7 downto 0 do
    print_string (string_of_int (8 - i) ^ " ");
    for j = 7 downto 0 do
      print_an_element board.(i).(j)
    done;
    print_endline ""
  done;

  print_string "  ";
  print_string "h g f e d c b a ";
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

let helper_king_knight board color r c =
  match side_piece (what_piece board r c) with
  | Nothing -> true
  | White -> if color = White then false else true
  | Black -> if color = Black then false else true

(** [general_moves_knight r c] is an array of coordinates of general moves by a
    knight at row r and column c*)
let general_moves_knight board r c =
  let new_inside_arr = ref [||] in
  let color = side_piece (what_piece board r c) in
  if r - 2 >= 0 && c + 1 < 8 && helper_king_knight board color (r - 2) (c + 1)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 2, c + 1))
  else ();
  if r - 1 >= 0 && c + 2 < 8 && helper_king_knight board color (r - 1) (c + 2)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 1, c + 2))
  else ();
  if r + 1 < 8 && c + 2 < 8 && helper_king_knight board color (r + 1) (c + 2)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 1, c + 2))
  else ();
  if r + 2 < 8 && c + 1 < 8 && helper_king_knight board color (r + 2) (c + 1)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 2, c + 1))
  else ();
  if r + 2 < 8 && c - 1 >= 0 && helper_king_knight board color (r + 2) (c - 1)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 2, c - 1))
  else ();
  if r + 1 < 8 && c - 2 >= 0 && helper_king_knight board color (r + 1) (c - 2)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 1, c - 2))
  else ();
  if r - 1 >= 0 && c - 2 >= 0 && helper_king_knight board color (r - 1) (c - 2)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 1, c - 2))
  else ();
  if r - 2 >= 0 && c - 1 >= 0 && helper_king_knight board color (r - 2) (c - 1)
  then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 2, c - 1))
  else ();
  !new_inside_arr

(** [general_moves_bishop r c] is an array of coordinates of general moves by a
    bishop at row r and column c*)
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
let general_moves_pawn_white board new_inside_arr r c =
  if side_piece (what_piece board (r - 1) c) = Nothing then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 1, c));
  if
    r = 6
    && side_piece (what_piece board (r - 1) c) = Nothing
    && side_piece (what_piece board (r - 2) c) = Nothing
  then new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r - 2, c))
  else if r <> 6 then (
    let left =
      if c - 1 >= 0 then
        match side_piece (what_piece board (r - 1) (c - 1)) with
        | White -> ()
        | Black ->
            new_inside_arr :=
              Array.append !new_inside_arr (Array.make 1 (r - 1, c - 1))
        | Nothing -> ()
      else ()
    in
    let right =
      if c + 1 < 8 then
        match side_piece (what_piece board (r - 1) (c + 1)) with
        | White -> ()
        | Black ->
            new_inside_arr :=
              Array.append !new_inside_arr (Array.make 1 (r - 1, c + 1))
        | Nothing -> ()
      else ()
    in
    right;
    left)

let general_moves_pawn_black board new_inside_arr r c =
  if side_piece (what_piece board (r + 1) c) = Nothing then
    new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 1, c));
  if
    r = 1
    && side_piece (what_piece board (r + 1) c) = Nothing
    && side_piece (what_piece board (r + 2) c) = Nothing
  then new_inside_arr := Array.append !new_inside_arr (Array.make 1 (r + 2, c))
  else if r <> 1 then (
    let right =
      if c + 1 < 8 then
        match side_piece (what_piece board (r + 1) (c + 1)) with
        | White ->
            new_inside_arr :=
              Array.append !new_inside_arr (Array.make 1 (r + 1, c + 1))
        | Black -> ()
        | Nothing -> ()
      else ()
    in
    let left =
      if c - 1 >= 0 then
        match side_piece (what_piece board (r + 1) (c - 1)) with
        | White ->
            new_inside_arr :=
              Array.append !new_inside_arr (Array.make 1 (r + 1, c - 1))
        | Black -> ()
        | Nothing -> ()
      else ()
    in
    right;
    left)

let reset_turns_en_passant board whites_turn_bool =
  for i = 0 to 7 do
    for j = 0 to 7 do
      let target = what_piece board i j in
      if
        target.side = White = whites_turn_bool
        && target.rank = Pawn && target.enPassant = true
      then board.(i).(j) <- { target with enPassant = false }
    done
  done;
  ()

let en_passant board ini_row ini_col dest_row dest_col =
  let initial = what_piece board ini_row ini_col in
  let color = side_piece (what_piece board ini_row ini_col) in
  match color with
  | White ->
      if dest_row - ini_row = -1 && dest_col - ini_col = 1 then
        let target = what_piece board ini_row (ini_col + 1) in
        if side_piece target <> color && target.enPassant then (
          remove_piece board ini_row ini_col;
          remove_piece board ini_row (ini_col + 1);
          place_piece board initial dest_row dest_col true;
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
      else if dest_row - ini_row = -1 && dest_col - ini_col = -1 then
        let target = what_piece board ini_row (ini_col - 1) in
        if side_piece target <> color && target.enPassant then (
          remove_piece board ini_row ini_col;
          remove_piece board ini_row (ini_col - 1);
          place_piece board initial dest_row dest_col true;
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
      else false
  | Black ->
      if dest_row - ini_row = 1 && dest_col - ini_col = 1 then
        let target = what_piece board ini_row (ini_col + 1) in
        if side_piece target <> color && target.enPassant then (
          remove_piece board ini_row ini_col;
          remove_piece board ini_row (ini_col + 1);
          place_piece board initial dest_row dest_col true;
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
      else if dest_row - ini_row = 1 && dest_col - ini_col = -1 then
        let target = what_piece board ini_row (ini_col - 1) in
        if side_piece target <> color && target.enPassant then (
          remove_piece board ini_row ini_col;
          remove_piece board ini_row (ini_col - 1);
          place_piece board initial dest_row dest_col true;
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
      else false
  | Nothing -> false

(* if (dest_row - ini_row = 1) && (dest_col - ini_col = 1) then (let target =
   (what_piece board ini_row (ini_col + 1)) in if (side_piece target) <> color
   && target.enPassant) then remove_piece board ini_row ini_col; remove_piece
   board ini_row (ini_col + 1); place_piece board initial dest_row dest_col
   true; true) else if dest_row - ini_row = 1 && dest_col - ini_col = -1 then *)

let general_moves_pawn board r c =
  let new_inside_arr = ref [||] in
  let color = side_piece (what_piece board r c) in
  if color = White then general_moves_pawn_white board new_inside_arr r c
  else general_moves_pawn_black board new_inside_arr r c;
  !new_inside_arr

(** [general_moves_king r c] is an array of coordinates of general moves by a
    king at row r and column c *)
let general_moves_king board r c =
  let new_inside_arr = ref [||] in
  let color = side_piece (what_piece board r c) in
  for row = max (r - 1) 0 to min (r + 1) 7 do
    for column = max (c - 1) 0 to min (c + 1) 7 do
      if row = r && column = c then ()
      else if helper_king_knight board color row column then
        new_inside_arr :=
          Array.append !new_inside_arr (Array.make 1 (row, column))
      else ()
    done
  done;
  !new_inside_arr

(** [legal_moves_knight r c] is an array of coordinates of legal moves by a
    knight at row r and column c *)
let legal_moves_knight (board : piece array array) r c =
  general_moves_knight board r c |> moves_except_outside

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
  for i = 0 to total_length_for_bishop_array - 1 do
    array.(i) <- (general_moves_bishop board r c).(i)
  done;
  for j = 0 to total_length_for_rook_array - 1 do
    array.(j + total_length_for_bishop_array) <-
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

let is_king_in_check board (side : side) =
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
    let captured_piece = what_piece board destination_row destination_column in
    remove_piece board piece_row piece_column;
    place_piece board mov_piece destination_row destination_column true;
    if is_king_in_check board (side_piece mov_piece) then (
      remove_piece board destination_row destination_column;
      place_piece board captured_piece destination_row destination_column
        captured_piece.hasMoved;
      place_piece board mov_piece piece_row piece_column mov_piece.hasMoved;
      false)
    else (
      if
        (what_piece board destination_row destination_column).rank = Pawn
        && abs (destination_row - piece_row) = 2
      then set_en_passant board destination_row destination_column;
      true))
  else false

(** master function for chess move*)
let move board (piece_row : int) (piece_column : int) (destination_row : int)
    (destination_column : int) =
  if
    side_piece (what_piece board piece_row piece_column) = White
    && !whites_turn
    || side_piece (what_piece board piece_row piece_column) = Black
       && !whites_turn = false
  then
    let piece =
      { (what_piece board piece_row piece_column) with hasMoved = true }
    in
    match rank_piece piece with
    | Knight ->
        if
          move_piece board legal_moves_knight piece_row piece_column
            destination_row destination_column
        then (
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
    | Bishop ->
        if
          move_piece board legal_moves_bishop piece_row piece_column
            destination_row destination_column
        then (
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
    | Rook ->
        if
          move_piece board legal_moves_rook piece_row piece_column
            destination_row destination_column
        then (
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
    | King ->
        if
          move_piece board legal_moves_king piece_row piece_column
            destination_row destination_column
        then (
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
    | Queen ->
        if
          move_piece board legal_moves_queen piece_row piece_column
            destination_row destination_column
        then (
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
    | Pawn ->
        if
          move_piece board legal_moves_pawn piece_row piece_column
            destination_row destination_column
        then (
          whites_turn := not !whites_turn;
          reset_turns_en_passant board !whites_turn;
          true)
        else false
    | _ -> false
  else false

let castle_helper board m_king m_rook king_row king_col rook_row rook_col =
  (* false if piece in given coordinates are not king and then rook*)
  if m_king.rank <> King || m_rook.rank <> Rook then false
    (* false if king and rook already moved*)
  else if hasMoved_piece m_king || hasMoved_piece m_rook then false
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
      (*case 1: rook --- king *)
      let in_or_pass_check = ref false in
      for i = king_col downto king_col - 2 do
        if is_king_in_check board m_king.side then in_or_pass_check := true
        else ();
        if i <> king_col - 2 then (
          remove_piece board king_row i;
          place_piece board m_king king_row (i - 1) true)
        else ()
      done;
      if !in_or_pass_check then (
        (*case 1 in/pass check: undo moving king*)
        remove_piece board king_row (king_col - 2);
        place_piece board m_king king_row king_col false;
        false)
      else (
        remove_piece board rook_row rook_col;
        place_piece board m_rook rook_row (rook_col + 3) true;
        m_king.hasMoved <- true;
        m_rook.hasMoved <- true;
        true))
    else if Int.max king_col rook_col = rook_col then (
      (*case 2: king --- rook*)
      let in_or_pass_check = ref false in
      for i = king_col to king_col + 2 do
        if is_king_in_check board m_king.side then in_or_pass_check := true
        else ();
        if i <> king_col + 2 then (
          remove_piece board king_row i;
          place_piece board m_king king_row (i + 1) true)
        else ()
      done;
      if !in_or_pass_check then (
        (*case 2 in/pass check: undo moving king*)
        remove_piece board king_row (king_col + 2);
        place_piece board m_king king_row king_col false;
        false)
      else (
        remove_piece board rook_row rook_col;
        place_piece board m_rook rook_row (rook_col - 2) true;
        m_king.hasMoved <- true;
        m_rook.hasMoved <- true;
        true))
    else false

let castle board (king_row : int) (king_col : int) (rook_row : int)
    (rook_col : int) =
  let maybe_king = what_piece board king_row king_col in
  let maybe_rook = what_piece board rook_row rook_col in
  (* false if the color of king and rook are different from color of the current
     turn*)
  if !whites_turn then
    if maybe_king.side <> White || maybe_rook.side <> White then false
    else if
      castle_helper board maybe_king maybe_rook king_row king_col rook_row
        rook_col
    then (
      whites_turn := not !whites_turn;
      reset_turns_en_passant board !whites_turn;
      true)
    else false
  else if maybe_king.side <> Black || maybe_rook.side <> Black then false
  else if
    castle_helper board maybe_king maybe_rook king_row king_col rook_row
      rook_col
  then (
    whites_turn := not !whites_turn;
    reset_turns_en_passant board !whites_turn;
    true)
  else false
