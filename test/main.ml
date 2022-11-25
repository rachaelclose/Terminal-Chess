open OUnit2
open Game
open Board

let what_piece_test (name : string) (board : board) (row : int) (col : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (what_piece board row col |> matching)

let move_test (name : string) (board : board) (orow : int) (ocol : int)
    (drow : int) (dcol : int) : test =
  name >:: fun _ ->
  let exp = what_piece board orow ocol |> matching in
  if move board orow ocol drow dcol then
    assert_equal exp (what_piece board drow dcol |> matching)

let what_piece_tests =
  [
    what_piece_test "1what_piece black rook" board_of_pieces 0 0 "♜";
    what_piece_test "2what_piece black knight" board_of_pieces 0 1 "♞";
    what_piece_test "3what_piece black bishop" board_of_pieces 0 2 "♝";
    what_piece_test "4what_piece black king" board_of_pieces 0 3 "♚";
    what_piece_test "5what_piece black queen" board_of_pieces 0 4 "♛";
    what_piece_test "6what_piece black bishop" board_of_pieces 0 5 "♝";
    what_piece_test "7what_piece black knight" board_of_pieces 0 6 "♞";
    what_piece_test "8what_piece black rook" board_of_pieces 0 0 "♜";
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

let move_tests = [ move_test "move white knight" board_of_pieces 7 6 5 5 ]

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
  >::: List.flatten [ what_piece_tests; move_tests (*remove_tests*) ]

let _ = run_test_tt_main suite
