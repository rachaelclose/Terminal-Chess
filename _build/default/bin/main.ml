(* print_endline ("Hi there!") --> this code will be printed out whenever we run
   <dune exec ./main.exe> under bin folder in terminal*)

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

(** [play_game f] starts the game in file [f]. *)
let play_game = init
(* raise (Failure "Unimplemented: Main.play_game") *)

(* let data_dir_prefix = "data" ^ Filename.dir_sep *)

(* [run_game] takes in a user_input to run the game. *)
let rec run_game board =
  ANSITerminal.print_string [ ANSITerminal.black ] board;
  match read_line () with
  | "move a7 a5" -> ANSITerminal.print_string [ ANSITerminal.black ] next_b
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.black ]
        "Please give a valid command"
if true then () else run_game next_b

(* create ~ command.ml in a2 -> parse (strip of spaces, etc) -> board.ml where
   you update board -> return that to main.ml*)

(** [main ()] prompts for the game to play, then starts it. *)
let main () = run_game play_game

(* "\n\nWelcome to the 3110 Chess Game engine.\n"; print_endline "Please enter
   the name of the game file you want to load.\n"; *)
(* print_string "> "; match read_line () with | exception End_of_file -> () |
   file_name -> play_game (data_dir_prefix ^ file_name ^ ".json") *)

(* Execute the game engine. *)
let () = main ()
