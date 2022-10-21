type piece
type board
type rank
type side

val remove_piece : board -> int -> int -> unit
val board_of_pawns : board
val board_of_spaces : board
val board_of_pieces : board
val rank_piece : piece -> rank
val side_piece : piece -> side
val matching : piece -> string
val move : board -> int -> int -> int -> int -> unit
val print_board : board -> unit
