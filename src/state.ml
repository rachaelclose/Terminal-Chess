open Board

type t = {
  current_board : piece array array;
  current_turn : string;
  under_check : string;
  previous_moves : string list;
  white_captured : string list;
  black_captured : string list;
}

let init_state board =
  {
    current_board = board;
    current_turn = "white";
    under_check = "none";
    previous_moves = [];
    white_captured = [];
    black_captured = [];
  }
