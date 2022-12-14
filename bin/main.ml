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
let row_int str index = 8 - (int_of_char (String.get str index) - 48)

(** turns column letters to numbers *)
let column_letter str index = int_of_char (String.get str index) - 97

(** tells if someone won / game is done*)
let game_end = ref false

(** check method that checks if it is a valid input*)
let parse board str =
  let nstr = String.split_on_char ' ' str |> List.filter (fun st -> st <> "") in
  match nstr with
  | [ h; t1; t2 ] ->
      if h = "move" then
        let c1 = column_letter t1 0 in
        let c2 = row_int t1 1 in
        let c3 = column_letter t2 0 in
        let c4 = row_int t2 1 in
        move board c2 c1 c4 c3
      else if h = "castle" then
        let c1 = column_letter t1 0 in
        let c2 = row_int t1 1 in
        let c3 = column_letter t2 0 in
        let c4 = row_int t2 1 in
        castle board c2 c1 c4 c3
      else if h = "en_passant" then
        let c1 = column_letter t1 0 in
        let c2 = row_int t1 1 in
        let c3 = column_letter t2 0 in
        let c4 = row_int t2 1 in
        en_passant board c2 c1 c4 c3
      else false
  | [ h ] ->
      if h = "withdraw" then (
        game_end := true;
        true)
      else if h = "checkmate" then (
        game_end := true;
        true)
      else if h = "help" then (
        print_endline
          "Commands: move c#1 c#2 where c is a char a to h and # is a number 1 \n\
          \              to 8 and c#1 represents the current row and column \
           and c#2 \n\
          \              represents the destination row and column\n\
          \          castle kcr rcr where kcr is the king's column and row in \n\
          \              the format c# and rcr is the rook's column and row in \
           the format c#\n\
          \          en_passant c#1 c#2 where c#1 is the current row and \n\
          \              column and c#2 is the destination row and column \n\
          \          withdraw is is used to withdraw from the game \n\
          \          checkmate is used to end the game when checkmate occurs\n\
          \          ";
        true)
      else false
  | _ -> false

(* add functions: is_checkmate; no_legal_moves_available;*)

(* update turn to <next_color>*)

(* For checkmate: *)
(* 1. check if <next_color>'s king is under check: if not return false*)
(* 2. else return if no_legal_moves*)

(* [run_game] takes in a user_input to run the game. *)
let rec run_game board =
  (* ANSITerminal.print_string [ ANSITerminal.black ] next_b; *)
  if !whites_turn then print_board_white board else print_board_black board;
  (* change next_b to output of function that gives a string form of board*)
  let s = read_line () in
  let p = parse board s in
  if p then () else print_endline "Please give a valid input";
  if !game_end then print_endline "Player Checkmated or Withdrew. Game Over"
  else run_game board
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
