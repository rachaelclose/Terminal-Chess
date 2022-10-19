type rank =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King

type side =
  | White
  | Black

type piece = {
  rank : rank;
  side : side;
}

type mov = {
  letter : char;
  number : int;
}
