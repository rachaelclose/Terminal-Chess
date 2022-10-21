type board
type piece
type rank
type side

val remove_piece : board -> int -> int -> unit
val board_of_pawns : piece array array
val rank_piece : piece -> rank
val side_piece : piece -> side