type rank
type side
type piece
type board

val what_piece : board -> int -> int -> piece
(** [what_piece board row_index column_index] is the piece at row_index and column_index in board*)
val board_of_pieces : board
(** [board_of_piece] is a board consisting of all the chess pieces in the correct ordering and placement*)
val rank_piece : piece -> rank
(** [rank_piece] is the rank field of the piece. (ex. bishop, rook) *)
val side_piece : piece -> side
(** [side_piece] is the side field of the piece. (ex. black, white) *)
val matching : piece -> string
(** [matching piece] matches piece with the corresponding representing charater*)
val move : board -> int -> int -> int -> int -> bool
(** [move piece_row piece_column destination_row destination_column] moves the piece at piece_row and piece_column to the location destination_row and destination_column*)
val print_board : board -> unit
(** [print_board board] prints out board*)
val castle: board -> int -> int -> int -> int -> bool
(** [castle board king_row king_col rook_row rook_col] castles the king and rook *)
val board_of_game : unit