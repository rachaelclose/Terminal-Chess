type rank
type side
type piece
type board

val what_piece : board -> int -> int -> piece
val remove_piece : board -> int -> int -> unit
val board_of_pawns : board
val board_of_spaces : board
val board_of_pieces : board
val rank_piece : piece -> rank
val side_piece : piece -> side
val matching : piece -> string
val move : board -> int -> int -> int -> int -> bool
val print_board : board -> unit
