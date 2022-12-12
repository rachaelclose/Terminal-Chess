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

let move_piece_test (name : string) (board : board) (orow : int) (ocol : int)
    (drow : int) (dcol : int) : test =
  name >:: fun _ ->
  let exp = what_piece board orow ocol |> matching in
  if move board orow ocol drow dcol then
    assert_equal exp (what_piece board drow dcol |> matching)

let move_legal_test (name : string) (board : board) (orow : int) (ocol : int)
    (drow : int) (dcol : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (move board orow ocol drow dcol)

let legal_test = board_of_pieces

let what_piece_tests =
  [
    what_piece_test "1what_piece black rook" board_of_pieces 0 0 "♜";
    what_piece_test "2what_piece black knight" board_of_pieces 0 1 "♞";
    what_piece_test "3what_piece black bishop" board_of_pieces 0 2 "♝";
    what_piece_test "4what_piece black king" board_of_pieces 0 3 "♚";
    what_piece_test "5what_piece black queen" board_of_pieces 0 4 "♛";
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
    what_piece_test "12what_piece white king" board_of_pieces 7 3 "♔";
    what_piece_test "13what_piece white queen" board_of_pieces 7 4 "♕";
    what_piece_test "14what_piece white bishop" board_of_pieces 7 5 "♗";
    what_piece_test "15what_piece white knight" board_of_pieces 7 6 "♘";
    what_piece_test "16what_piece white rook" board_of_pieces 7 7 "♖";
  ]

let move_piece_tests =
  [
    move_piece_test "move white knight left" board_of_pieces 7 6 5 5;
    move_piece_test "move white knight right" board_of_pieces 7 6 5 7;
    move_piece_test "move white knight right left" board_of_pieces 5 7 4 5;
    move_piece_test "move black pawn" board_of_pieces 1 3 3 3;
    move_piece_test "move black bishop" board_of_pieces 0 2 1 3;
    move_piece_test "move black bishop again" board_of_pieces 1 3 2 4;
    move_piece_test "move black bishop back" board_of_pieces 2 4 1 3;
    move_piece_test "move white pawn" board_of_pieces 6 7 4 7;
    move_piece_test "move white rook" board_of_pieces 7 7 3 7;
  ]

let reset _ = board_of_game

let move_legal_tests =
  [
    move_legal_test "move white knight legal" board_of_pieces 7 6 5 5 true;
    move_legal_test "move black rook illegal" board_of_pieces 0 0 1 0 false;
    move_legal_test "move white knight illegal" board_of_pieces 5 5 4 6 false;
    move_legal_test "move white knight illegal 2!" board_of_pieces 5 5 7 4 false;
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
    move_legal_test "move black pawn 5 illegal" board_of_pieces 1 5 3 5 false;
    move_legal_test "move black pawn 5" board_of_pieces 1 5 2 5 true;
    move_legal_test "move black pawn 6" board_of_pieces 1 6 2 6 true;
    move_legal_test "move black pawn 7" board_of_pieces 1 7 3 7 true;
    move_legal_test "move black rook 1" board_of_pieces 0 0 2 0 true;
    move_legal_test "move black rook 2" board_of_pieces 2 0 1 0 true;
    move_legal_test "move black rook 3" board_of_pieces 1 0 1 5 true;
    move_legal_test "move black rook 4 illegal" board_of_pieces 1 5 2 5 false;
    move_legal_test "move black knight illegal 1" board_of_pieces 0 1 0 1 false;
    move_legal_test "move black knight illegal 2" board_of_pieces 0 1 0 3 false;
    move_legal_test "move black knight illegal 3" board_of_pieces 0 1 2 2 false;
    move_legal_test "move black knight" board_of_pieces 0 1 2 0 true;
    move_legal_test "move black rook right illegal 1" board_of_pieces 0 7 0 0
      false;
    move_legal_test "move black rook right illegal 2" board_of_pieces 0 7 1 6
      false;
    move_legal_test "move black rook right" board_of_pieces 0 7 2 7 true;
    move_legal_test "move black rook left" board_of_pieces 1 5 1 0 true;
    move_legal_test "move black knight right" board_of_pieces 0 6 1 4 true;
    move_legal_test "move black knight right illegal 1" board_of_pieces 1 4 1 4
      false;
    move_legal_test "move black knight right illegal 2" board_of_pieces 1 4 3 5
      false;
    move_legal_test "move black knight right illegal 3" board_of_pieces 1 4 3 3
      true;
    move_legal_test "move black bishop right illegal 1" board_of_pieces 0 5 2 7
      false;
    move_legal_test "move black bishop right illegal 2" board_of_pieces 0 5 0 4
      false;
    move_legal_test "move black bishop right illegal 3" board_of_pieces 0 5 0 7
      false;
    move_legal_test "move black bishop right illegal 4" board_of_pieces 0 5 1 0
      false;
    move_legal_test "move black bishop right illegal 5" board_of_pieces 0 5 2 5
      false;
    move_legal_test "move black bishop right 1" board_of_pieces 0 5 5 0 true;
    move_legal_test "move black bishop right 2" board_of_pieces 5 0 6 1 true;
    move_legal_test "move black bishop right 3" board_of_pieces 6 1 5 2 true;
    move_legal_test "move black bishop right illegal 6" board_of_pieces 5 2 4 3
      false;
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
    move_legal_test "move black pawn illegal mate" board_of_pieces 3 7 4 7 false
    (*8/r3k3/n1p1qppr/pp1npb1p/3p4/B1b2N2/P1PPPPPP/RN1QKB1R w KQkq - 0 1*);
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
