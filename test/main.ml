(* Test Plan: - We are automatically testing the functions what_piece and move,
   thus implicitly testing board_of_pieces, matching, and many helper functions
   that describe the moves of each chess piece. These tests were done through
   black box and glass box testing. - We are manually testing print_board
   through the interactive game on the terminal. - We have proven the
   correctness of our system by comparing the output of our functions with the
   expected outputs, as well as physically seeing if our system works the way it
   is supposed through our interactive terminal.*)
open OUnit2
open Game
open Board

let what_piece_test (name : string) (board : board) (row : int) (col : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (what_piece board row col |> matching)

let side_piece_test (name : string) (char : string) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (unmatching char |> side_piece |> match_side)

let move_piece_test (name : string) (board : board) (orow : int) (ocol : int)
    (drow : int) (dcol : int) : test =
  name >:: fun _ ->
  let exp = what_piece board orow ocol |> matching in
  if move board orow ocol drow dcol then
    assert_equal exp (what_piece board drow dcol |> matching)

let move_legal_test (name : string) (board : board) (orow : int) (ocol : int)
    (drow : int) (dcol : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (move board orow ocol drow dcol)
    ~printer:string_of_bool

(* let legal_test = board_of_pieces *)

let what_piece_tests =
  [
    what_piece_test "1what_piece black rook" board_of_pieces 0 0 "♜";
    what_piece_test "2what_piece black knight" board_of_pieces 0 1 "♞";
    what_piece_test "3what_piece black bishop" board_of_pieces 0 2 "♝";
    what_piece_test "4what_piece black king" board_of_pieces 0 4 "♚";
    what_piece_test "5what_piece black queen" board_of_pieces 0 3 "♛";
    what_piece_test "6what_piece black bishop" board_of_pieces 0 5 "♝";
    what_piece_test "7what_piece black knight" board_of_pieces 0 6 "♞";
    what_piece_test "8what_piece black rook" board_of_pieces 0 7 "♜";
    what_piece_test "9what_piece black pawn" board_of_pieces 1 0 "♟";
    what_piece_test "10what_piece black pawn" board_of_pieces 1 1 "♟";
    what_piece_test "11what_piece black pawn" board_of_pieces 1 2 "♟";
    what_piece_test "12what_piece black pawn" board_of_pieces 1 3 "♟";
    what_piece_test "13what_piece black pawn" board_of_pieces 1 4 "♟";
    what_piece_test "14what_piece black pawn" board_of_pieces 1 5 "♟";
    what_piece_test "15what_piece black pawn" board_of_pieces 1 6 "♟";
    what_piece_test "16what_piece black pawn" board_of_pieces 1 7 "♟";
    what_piece_test "1what_piece empty space" board_of_pieces 2 0 "_";
    what_piece_test "2what_piece empty space" board_of_pieces 2 1 "_";
    what_piece_test "3what_piece empty space" board_of_pieces 2 2 "_";
    what_piece_test "4what_piece empty space" board_of_pieces 2 3 "_";
    what_piece_test "5what_piece empty space" board_of_pieces 2 4 "_";
    what_piece_test "6what_piece empty space" board_of_pieces 2 5 "_";
    what_piece_test "7what_piece empty space" board_of_pieces 2 6 "_";
    what_piece_test "8what_piece empty space" board_of_pieces 2 7 "_";
    what_piece_test "9what_piece empty space" board_of_pieces 3 0 "_";
    what_piece_test "10what_piece empty space" board_of_pieces 3 1 "_";
    what_piece_test "11what_piece empty space" board_of_pieces 3 2 "_";
    what_piece_test "12what_piece empty space" board_of_pieces 3 3 "_";
    what_piece_test "13what_piece empty space" board_of_pieces 3 4 "_";
    what_piece_test "14what_piece empty space" board_of_pieces 3 5 "_";
    what_piece_test "15what_piece empty space" board_of_pieces 3 6 "_";
    what_piece_test "16what_piece empty space" board_of_pieces 3 7 "_";
    what_piece_test "17what_piece empty space" board_of_pieces 4 0 "_";
    what_piece_test "18what_piece empty space" board_of_pieces 4 1 "_";
    what_piece_test "19what_piece empty space" board_of_pieces 4 2 "_";
    what_piece_test "20what_piece empty space" board_of_pieces 4 3 "_";
    what_piece_test "21what_piece empty space" board_of_pieces 4 4 "_";
    what_piece_test "22what_piece empty space" board_of_pieces 4 5 "_";
    what_piece_test "23what_piece empty space" board_of_pieces 4 6 "_";
    what_piece_test "24what_piece empty space" board_of_pieces 4 7 "_";
    what_piece_test "25what_piece empty space" board_of_pieces 5 0 "_";
    what_piece_test "26what_piece empty space" board_of_pieces 5 1 "_";
    what_piece_test "27what_piece empty space" board_of_pieces 5 2 "_";
    what_piece_test "28what_piece empty space" board_of_pieces 5 3 "_";
    what_piece_test "29what_piece empty space" board_of_pieces 5 4 "_";
    what_piece_test "30what_piece empty space" board_of_pieces 5 5 "_";
    what_piece_test "31what_piece empty space" board_of_pieces 5 6 "_";
    what_piece_test "32what_piece empty space" board_of_pieces 5 7 "_";
    what_piece_test "1what_piece white pawn" board_of_pieces 6 0 "♙";
    what_piece_test "2what_piece white pawn" board_of_pieces 6 1 "♙";
    what_piece_test "3what_piece white pawn" board_of_pieces 6 2 "♙";
    what_piece_test "4what_piece white pawn" board_of_pieces 6 3 "♙";
    what_piece_test "5what_piece white pawn" board_of_pieces 6 4 "♙";
    what_piece_test "6what_piece white pawn" board_of_pieces 6 5 "♙";
    what_piece_test "7what_piece white pawn" board_of_pieces 6 6 "♙";
    what_piece_test "8what_piece white pawn" board_of_pieces 6 7 "♙";
    what_piece_test "9what_piece white rook" board_of_pieces 7 0 "♖";
    what_piece_test "10what_piece white knight" board_of_pieces 7 1 "♘";
    what_piece_test "11what_piece white bishop" board_of_pieces 7 2 "♗";
    what_piece_test "12what_piece white king" board_of_pieces 7 4 "♔";
    what_piece_test "13what_piece white queen" board_of_pieces 7 3 "♕";
    what_piece_test "14what_piece white bishop" board_of_pieces 7 5 "♗";
    what_piece_test "15what_piece white knight" board_of_pieces 7 6 "♘";
    what_piece_test "16what_piece white rook" board_of_pieces 7 7 "♖";
  ]

let side_piece_tests =
  [
    side_piece_test "side piece white pawn" "♙" "white";
    side_piece_test "side piece white rook" "♖" "white";
    side_piece_test "side piece white knight" "♘" "white";
    side_piece_test "side piece white bishop" "♗" "white";
    side_piece_test "side piece white king" "♔" "white";
    side_piece_test "side piece white queen" "♕" "white";
    side_piece_test "side piece black pawn" "♟" "black";
    side_piece_test "side piece black rook" "♜" "black";
    side_piece_test "side piece black knight" "♞" "black";
    side_piece_test "side piece black bishop" "♝" "black";
    side_piece_test "side piece black king" "♚" "black";
    side_piece_test "side piece black queen" "♛" "black";
    side_piece_test "side piece black queen" "_" "none";
  ]

let reset test =
  board_of_game;
  test

let move_piece_tests =
  [
    reset (move_piece_test "moved white knight left" board_of_pieces 7 6 5 5);
    move_piece_test "moved white knight right" board_of_pieces 7 6 5 7;
    move_piece_test "moved white knight right left" board_of_pieces 5 7 4 5;
    move_piece_test "moved black pawn" board_of_pieces 1 3 3 3;
    move_piece_test "moved black bishop" board_of_pieces 0 2 1 3;
    move_piece_test "moved black bishop again" board_of_pieces 1 3 2 4;
    move_piece_test "moved black bishop back" board_of_pieces 2 4 1 3;
    move_piece_test "moved white pawn" board_of_pieces 6 7 4 7;
    move_piece_test "moved white rook" board_of_pieces 7 7 3 7;
   move_piece_test "moved white king" board_of_pieces 7 7 3 7;
  ]

let move_legal_tests =
  [
    reset (move_legal_test "move white knight illegal" board_of_pieces 7 1 7 0 false);
    move_legal_test "move white knight illegal" board_of_pieces 7 1 7 2 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 7 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 7 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 7 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 7 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 7 7 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 0 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 1 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 2 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 6 7 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 5 1 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 5 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 5 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 5 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 5 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 5 7 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 0 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 1 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 2 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 4 7 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 0 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 1 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 2 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 3 7 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 0 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 1 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 2 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 2 7 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 0 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 1 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 2 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 1 7 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 0 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 1 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 2 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 3 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 4 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 5 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 6 false;
    move_legal_test "move white knight illegal" board_of_pieces 7 1 0 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 0 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 7 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 0 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 6 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 5 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 5 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 5 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 5 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 5 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 5 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 5 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 4 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 4 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 4 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 4 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 4 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 4 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 4 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 0 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 3 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 0 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 2 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 0 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 1 7 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 0 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 1 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 2 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 3 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 4 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 5 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 6 false;
    move_legal_test "move white pawn 0 illegal" board_of_pieces 6 0 0 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 1 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 7 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 1 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 6 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 5 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 5 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 5 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 5 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 5 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 5 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 5 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 4 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 4 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 4 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 4 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 4 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 4 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 4 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 1 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 3 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 1 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 2 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 1 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 1 7 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 0 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 1 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 2 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 3 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 4 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 5 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 6 false;
    move_legal_test "move white pawn 1 illegal" board_of_pieces 6 1 0 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 2 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 7 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 2 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 6 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 5 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 5 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 5 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 5 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 5 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 5 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 5 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 4 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 4 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 4 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 4 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 4 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 4 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 4 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 2 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 3 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 2 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 2 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 2 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 1 7 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 0 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 1 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 2 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 3 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 4 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 5 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 6 false;
    move_legal_test "move white pawn 2 illegal" board_of_pieces 6 2 0 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 3 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 7 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 3 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 6 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 5 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 5 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 5 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 5 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 5 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 5 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 5 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 4 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 4 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 4 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 4 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 4 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 4 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 4 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 3 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 3 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 3 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 2 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 3 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 1 7 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 0 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 1 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 2 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 3 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 4 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 5 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 6 false;
    move_legal_test "move white pawn 3 illegal" board_of_pieces 6 3 0 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 4 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 7 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 4 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 6 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 5 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 5 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 5 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 5 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 5 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 5 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 5 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 4 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 4 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 4 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 4 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 4 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 4 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 4 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 4 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 3 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 4 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 2 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 4 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 1 7 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 0 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 1 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 2 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 3 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 4 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 5 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 6 false;
    move_legal_test "move white pawn 4 illegal" board_of_pieces 6 4 0 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 5 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 7 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 5 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 6 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 5 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 5 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 5 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 5 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 5 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 5 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 5 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 4 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 4 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 4 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 4 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 4 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 4 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 4 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 5 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 3 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 5 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 2 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 5 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 1 7 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 0 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 1 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 2 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 3 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 4 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 5 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 6 false;
    move_legal_test "move white pawn 5 illegal" board_of_pieces 6 5 0 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 6 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 7 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 6 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 6 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 5 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 5 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 5 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 5 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 5 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 5 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 5 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 4 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 4 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 4 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 4 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 4 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 4 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 4 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 6 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 3 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 6 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 2 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 6 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 1 7 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 0 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 1 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 2 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 3 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 4 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 5 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 6 false;
    move_legal_test "move white pawn 6 illegal" board_of_pieces 6 6 0 7 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 7 7 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 6 7 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 5 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 5 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 5 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 5 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 5 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 5 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 5 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 4 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 4 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 4 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 4 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 4 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 4 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 4 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 3 7 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 2 7 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 1 7 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 0 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 1 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 2 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 3 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 4 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 5 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 6 false;
    move_legal_test "move white pawn 7 illegal" board_of_pieces 6 7 0 7 false;
    move_legal_test "move white pawn 0 one space legal" board_of_pieces 6 0 5 0
      true;
     ();)

      move_legal_test "move white pawn 1 one space legal" board_of_pieces 6 1 5
        1 true;
      move_legal_test "move white pawn 2 one space legal" board_of_pieces 6 2 5
        2 true;
      move_legal_test "move white pawn 3 one space legal" board_of_pieces 6 3 5
        3 true;
      move_legal_test "move white pawn 4 one space legal" board_of_pieces 6 4 5
        4 true;
      move_legal_test "move white pawn 5 one space legal" board_of_pieces 6 5 5
        5 true;
      move_legal_test "move white pawn 6 one space legal" board_of_pieces 6 6 5
        6 true;
      move_legal_test "move white pawn 7 one space legal" board_of_pieces 6 7 5
        7 true;
      move_legal_test "move white pawn 0 two spaces legal" board_of_pieces 6 0 4
        0 true;
      move_legal_test "move white pawn 1 two spaces legal" board_of_pieces 6 1 4
        1 true;
      move_legal_test "move white pawn 2 two spaces legal" board_of_pieces 6 2 4
        2 true;
      move_legal_test "move white pawn 3 two spaces legal" board_of_pieces 6 3 4
        3 true;
      move_legal_test "move white pawn 4 two spaces legal" board_of_pieces 6 4 4
        4 true;
      move_legal_test "move white pawn 5 two spaces legal" board_of_pieces 6 5 4
        5 true;
      move_legal_test "move white pawn 6 two spaces legal" board_of_pieces 6 6 4
        6 true;
      move_legal_test "move white pawn 7 two spaces legal" board_of_pieces 6 7 4
        7 true;
      move_legal_test "move white knight legal" board_of_pieces 7 6 5 5 true;
      move_legal_test "move white knight legal" board_of_pieces 7 6 5 7 true;
      move_legal_test "move white knight legal" board_of_pieces 7 1 5 0 true;
      move_legal_test "move white knight legal" board_of_pieces 7 1 5 2 true;
      move_legal_test "move black rook illegal" board_of_pieces 0 0 1 0 false;
      move_legal_test "move white knight illegal" board_of_pieces 5 5 4 6 false;
      move_legal_test "move white knight illegal 2!" board_of_pieces 5 5 7 4
        false;
      (*possibly an error*)
      move_legal_test "move black bishop illegal" board_of_pieces 0 2 1 3 false;
      move_legal_test "double move black pawn" board_of_pieces 1 3 3 3 true;
      move_legal_test "move black pawn illegal" board_of_pieces 3 3 5 3 false;
      move_legal_test "move black pawn" board_of_pieces 3 3 4 3 true;
      move_legal_test "move black bishop" board_of_pieces 0 2 3 5 true;
      move_legal_test "move black bishop illegal" board_of_pieces 3 5 4 5 false;
      move_legal_test "move black pawn 0" board_of_pieces 1 0 3 0 true;
      move_legal_test "move black pawn 1" board_of_pieces 1 1 3 0 false;
      move_legal_test "move black pawn 1" board_of_pieces 1 1 4 1 false;
      move_legal_test "move black pawn 1" board_of_pieces 1 1 3 1 true;
      move_legal_test "move black pawn 2" board_of_pieces 1 2 0 2 false;
      move_legal_test "move black pawn 2" board_of_pieces 1 2 2 2 true;
      move_legal_test "move black pawn 4" board_of_pieces 1 4 3 4 true;
      move_legal_test "move black pawn 5 double" board_of_pieces 1 5 3 5 true;
      (*possibly an error*)
      move_legal_test "move black pawn 5" board_of_pieces 1 5 2 5 true;
      move_legal_test "move black pawn 6" board_of_pieces 1 6 2 6 true;
      move_legal_test "move black pawn 7" board_of_pieces 1 7 3 7 true;
      move_legal_test "move black rook 1" board_of_pieces 0 0 2 0 true;
      move_legal_test "move black rook 2" board_of_pieces 2 0 1 0 true;
      move_legal_test "move black rook 3" board_of_pieces 1 0 1 5 true;
      move_legal_test "move black rook 4 illegal" board_of_pieces 1 5 2 5 false;
      move_legal_test "move black knight illegal 1" board_of_pieces 0 1 0 1
        false;
      move_legal_test "move black knight illegal 2" board_of_pieces 0 1 0 3
        false;
      move_legal_test "move black knight illegal 3" board_of_pieces 0 1 2 2
        false;
      move_legal_test "move black knight" board_of_pieces 0 1 2 0 true;
      move_legal_test "move black rook right illegal 1" board_of_pieces 0 7 0 0
        false;
      move_legal_test "move black rook right illegal 2" board_of_pieces 0 7 1 6
        false;
      move_legal_test "move black rook right" board_of_pieces 0 7 2 7 true;
      move_legal_test "move black rook left" board_of_pieces 1 5 1 0 true;
      move_legal_test "move black knight right" board_of_pieces 0 6 1 4 true;
      move_legal_test "move black knight right illegal 1" board_of_pieces 1 4 1
        4 false;
      move_legal_test "move black knight right illegal 2" board_of_pieces 1 4 3
        5 false;
      move_legal_test "move black knight right illegal 3" board_of_pieces 1 4 3
        3 true;
      move_legal_test "move black bishop right illegal 1" board_of_pieces 0 5 2
        7 false;
      move_legal_test "move black bishop right illegal 2" board_of_pieces 0 5 0
        4 false;
      move_legal_test "move black bishop right illegal 3" board_of_pieces 0 5 0
        7 false;
      move_legal_test "move black bishop right illegal 4" board_of_pieces 0 5 1
        0 false;
      move_legal_test "move black bishop right illegal 5" board_of_pieces 0 5 2
        5 false;
      move_legal_test "move black bishop right 1" board_of_pieces 0 5 5 0 true;
      move_legal_test "move black bishop right 2" board_of_pieces 5 0 6 1 true;
      move_legal_test "move black bishop right 3" board_of_pieces 6 1 5 2 true;
      move_legal_test "move black bishop right illegal 6" board_of_pieces 5 2 4
        3 false;
      move_legal_test "move black queen illegal 1" board_of_pieces 0 3 0 4 false;
      move_legal_test "move black queen illegal 2" board_of_pieces 0 3 2 5 false;
      move_legal_test "move black queen 1" board_of_pieces 0 3 0 0 true;
      move_legal_test "move black queen illegal 1" board_of_pieces 0 0 1 0 false;
      move_legal_test "move black queen illegal 2" board_of_pieces 0 0 0 4 false;
      move_legal_test "move black queen illegal 3" board_of_pieces 0 0 2 2 false;
      move_legal_test "move black queen 2" board_of_pieces 0 0 0 1 true;
      move_legal_test "move black queen 3" board_of_pieces 0 1 1 1 true;
      move_legal_test "move black queen 4" board_of_pieces 1 1 0 2 true;
      move_legal_test "move black queen 5" board_of_pieces 0 2 2 4 true;
      move_legal_test "move black king 1" board_of_pieces 0 4 0 3 true;
      move_legal_test "move black king 2" board_of_pieces 0 3 1 2 true;
      move_legal_test "move black king 3" board_of_pieces 1 2 1 3 true;
      move_legal_test "move black king 4" board_of_pieces 1 3 2 3 true;
      move_legal_test "move black king 5" board_of_pieces 2 3 1 4 true;
      move_legal_test "move black king illegal 1" board_of_pieces 1 4 2 4 false;
      move_legal_test "move white bishop" board_of_pieces 7 2 5 0 true;
      move_legal_test "move black pawn illegal mate" board_of_pieces 3 7 4 7
        false
      (*8/r3k3/n1p1qppr/pp1npb1p/3p4/B1b2N2/P1PPPPPP/RN1QKB1R w KQkq - 0 1*));
  ]

(*let remove_tests = let removed_blk_king = remove_piece board_of_pieces 3 0 in
  let removed_blk_pawn = remove_piece board_of_pieces 1 3 in let removed_nothing
  = remove_piece board_of_pieces 4 3 in let removed_white_bishop = remove_piece
  board_of_pieces 7 2 in [ what_piece_test "what_piece of removed black king"
  board_of_pieces 3 0 "_"; what_piece_test "what_piece of removed black pawn"
  board_of_pieces 1 3 "_"; what_piece_test "what_piece of removed nothing"
  board_of_pieces 4 3 "_"; what_piece_test "what_piece of removed white bishop"
  board_of_pieces 7 2 "_"; ]*)

let suite =
  "test suite for chess"
  >::: List.flatten
         [
           what_piece_tests; move_piece_tests; move_legal_tests (*remove_tests*);
         ]

let _ = run_test_tt_main suite
