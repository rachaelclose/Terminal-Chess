(* print_endline ("Hi there!") --> this code will be printed out whenever we run
   <dune exec ./main.exe> under bin folder in terminal*)

open Game.Board
(** initial board*)

let init =
  "   a b c d e f g h \n\
  \ 1 ♜ ♞ ♝ ♚ ♛ ♝ ♞ ♜ \n\
  \ 2 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ \n\
  \ 3 _ _ _ _ _ _ _ _ \n\
  \ 4 _ _ _ _ _ _ _ _ \n\
  \ 5 _ _ _ _ _ _ _ _ \n\
  \ 6 _ _ _ _ _ _ _ _ \n\
  \ 7 ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ \n\
  \ 8 ♖ ♘ ♗ ♔ ♕ ♗ ♘ ♖ \n"

let next_b =
  "   a b c d e f g h \n\
  \ 1 ♜ ♞ ♝ ♚ ♛ ♝ ♞ ♜ \n\
  \ 2 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ \n\
  \ 3 _ _ _ _ _ _ _ _ \n\
  \ 4 _ _ _ _ _ _ _ _ \n\
  \ 5 ♙ _ _ _ _ _ _ _ \n\
  \ 6 _ _ _ _ _ _ _ _ \n\
  \ 7 _ ♙ ♙ ♙ ♙ ♙ ♙ ♙ \n\
  \ 8 ♖ ♘ ♗ ♔ ♕ ♗ ♘ ♖ \n"

(** match piece with the corresponding representing letter*)
  let matching piece = 
   match piece.rank with 
   | Pawn -> "p"
   | Bishop -> "b"
   | Knight -> "k"
   | Rook -> "r"
   | Queen -> "q"
   | King -> "K"
   | Nothing -> "_"
   
(** print a single piece's letter*)
   let print_an_element piece = 
    print_string ((matching piece) ^ " ")
   
(** print the current board*)
   let print_board board = 
   board |> Array.iter (fun x -> Array.iter print_an_element x; print_endline "")
   

(** [play_game f] starts the game in file [f]. *)
let play_game = init
(* raise (Failure "Unimplemented: Main.play_game") *)

(* let data_dir_prefix = "data" ^ Filename.dir_sep *)

(** check method that checks if it is a valid input*)
let check str = true

(** variable showing if someone won / game is done*)
let game_end = false

(** method updating the board*)
let update board s = remove_piece board 3 4

(* [run_game] takes in a user_input to run the game. *)
let rec run_game board =
  ANSITerminal.print_string [ ANSITerminal.black ] next_b;
  (* change next_b to output of function that gives a string form of board*)
  let s = read_line () in
  let p = check s in
  if p then update board s else print_endline "Please give a valid input";
  if game_end then print_endline "Congrats you won" else run_game board
(* match read_line () with | "move a7 a5" -> ANSITerminal.print_string [
   ANSITerminal.black ] next_b | _ -> ANSITerminal.print_string [
   ANSITerminal.black ] "Please give a valid command" *)

(* create ~ command.ml in a2 -> parse (strip of spaces, etc) -> board.ml where
   you update board -> return that to main.ml*)

(** [main ()] prompts for the game to play, then starts it. *)
let main () = run_game board_of_pawns

(* "\n\nWelcome to the 3110 Chess Game engine.\n"; print_endline "Please enter
   the name of the game file you want to load.\n"; *)
(* print_string "> "; match read_line () with | exception End_of_file -> () |
   file_name -> play_game (data_dir_prefix ^ file_name ^ ".json") *)

(* Execute the game engine. *)
let () = main ()
