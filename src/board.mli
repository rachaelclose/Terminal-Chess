type rank
type side
type piece
type board

val what_piece : board -> int -> int -> piece
(** [what_piece board row_index column_index] is the piece at row_index and
    column_index in board*)

val board_of_pieces : board
(** [board_of_pieces] is a board consisting of all the chess pieces in the
    correct ordering and placement*)

val rank_piece : piece -> rank
(** [rank_piece] is the rank field of the piece. (ex. bishop, rook) *)

val side_piece : piece -> side
(** [side_piece] is the side field of the piece. (ex. black, white) *)

val matching : piece -> string
(** [matching piece] matches piece with the corresponding representing charater*)

val unmatching : string -> piece
(** [unmatching char] matches char containing a chess character with a piece*)

val match_side : side -> string
(** [match_side side] matches side with the corresponding representing string*)

val move : board -> int -> int -> int -> int -> bool
(** [move piece_row piece_column destination_row destination_column] moves the
    piece at piece_row and piece_column to the location destination_row and
    destination_column*)

val print_board_white : board -> unit
(** [print_board_white board] prints out board when it's white's turn*)

val print_board_black : board -> unit
(** [print_board_black board] prints out board when it's black's turn*)

val castle : board -> int -> int -> int -> int -> bool
(** [castle board king_row king_col rook_row rook_col] castles the king and rook *)

val en_passant : board -> int -> int -> int -> int -> bool
(** [en_passant board source_row source_col dest_row dest_col] attempts en
    passant on the given pawn *)

val board_of_game : unit
(** [board_of_game] sets the board *)

val whites_turn : bool ref
(** [whites_turn] indicates whether's it's white's turn to move or not *)