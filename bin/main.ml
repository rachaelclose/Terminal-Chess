(* print_endline ("Hi there!") --> this code will be printed out whenever we run
   <dune exec ./main.exe> under bin folder in terminal*)
(**)

open Game.Board

open Printf
(** initial board*)

let init =
  "   a b c d e f g h \n\
  \ 8 ♜ ♞ ♝ ♚ ♛ ♝ ♞ ♜ \n\
  \ 7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ \n\
  \ 6 _ _ _ _ _ _ _ _ \n\
  \ 5 _ _ _ _ _ _ _ _ \n\
  \ 4 _ _ _ _ _ _ _ _ \n\
  \ 3 _ _ _ _ _ _ _ _ \n\
  \ 2 ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ \n\
  \ 1 ♖ ♘ ♗ ♔ ♕ ♗ ♘ ♖ \n"

let next_b =
  "   a b c d e f g h \n\
  \ 8 ♜ ♞ ♝ ♚ ♛ ♝ ♞ ♜ \n\
  \ 7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ \n\
  \ 6 _ _ _ _ _ _ _ _ \n\
  \ 5 _ _ _ _ _ _ _ _ \n\
  \ 4 ♙ _ _ _ _ _ _ _ \n\
  \ 3 _ _ _ _ _ _ _ _ \n\
  \ 2 _ ♙ ♙ ♙ ♙ ♙ ♙ ♙ \n\
  \ 1 ♖ ♘ ♗ ♔ ♕ ♗ ♘ ♖ \n"

(** [play_game f] starts the game in file [f]. *)
let play_game = init
(* raise (Failure "Unimplemented: Main.play_game") *)

(* let data_dir_prefix = "data" ^ Filename.dir_sep *)

(** check method that checks if it is a valid input*)
let s_int str index = int_of_char (String.get str index) - 48

(** check method that checks if it is a valid input*)
let parse board str =
  let nstr = String.split_on_char ' ' str |> List.filter (fun st -> st <> "") in
  match nstr with
  | [ h; t1; t2 ] ->
      if h = "move" then
        let c1 = s_int t1 0 in
        let c2 = s_int t1 1 in
        let c3 = s_int t2 0 in
        let c4 = s_int t2 1 in
        move board c1 c2 c3 c4
      else false
  | _ -> false

(** variable showing if someone won / game is done*)
let game_end = false

(** method updating the board*)
let update board s = remove_piece board 3 4

(* [run_game] takes in a user_input to run the game. *)
let rec run_game board =
  (* ANSITerminal.print_string [ ANSITerminal.black ] next_b; *)
  print_board board;
  (* change next_b to output of function that gives a string form of board*)
  let s = read_line () in
  let p = parse board s in
  if p then () else print_endline "Please give a valid input";
  if game_end then print_endline "Congrats you won" else run_game board
(* match read_line () with | "move a7 a5" -> ANSITerminal.print_string [
   ANSITerminal.black ] next_b | _ -> ANSITerminal.print_string [
   ANSITerminal.black ] "Please give a valid command" *)

(* create ~ command.ml in a2 -> parse (strip of spaces, etc) -> board.ml where
   you update board -> return that to main.ml*)

(** [main ()] prompts for the game to play, then starts it. *)
let main () = run_game board_of_pieces

(* "\n\nWelcome to the 3110 Chess Game engine.\n"; print_endline "Please enter
   the name of the game file you want to load.\n"; *)
(* print_string "> "; match read_line () with | exception End_of_file -> () |
   file_name -> play_game (data_dir_prefix ^ file_name ^ ".json") *)

(* Execute the game engine. *)
let () = main ()
