(* print_endline ("Hi there!") --> this code will be printed out whenever we run
   <dune exec ./main.exe> under bin folder in terminal*)

(** initial board*)

let init =
  "   a b c d e f g h \n\
  \ 1 _ _ _ _ _ _ _ _ \n\
  \ 2 _ _ _ _ _ _ _ _ \n\
  \ 3 _ _ _ _ _ _ _ _ \n\
  \ 4 _ _ _ _ _ _ _ _ \n\
  \ 5 _ _ _ _ _ _ _ _ \n\
  \ 6 _ _ _ _ _ _ _ _ \n\
  \ 7 _ _ _ _ _ _ _ _ \n\
  \ 8 _ _ _ _ _ _ _ _ \n"

(** [play_game f] starts the game in file [f]. *)
let play_game = init
(* raise (Failure "Unimplemented: Main.play_game") *)

(* let data_dir_prefix = "data" ^ Filename.dir_sep *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () = ANSITerminal.print_string [ ANSITerminal.black ] play_game

(* "\n\nWelcome to the 3110 Chess Game engine.\n"; print_endline "Please enter
   the name of the game file you want to load.\n"; *)
(* print_string "> "; match read_line () with | exception End_of_file -> () |
   file_name -> play_game (data_dir_prefix ^ file_name ^ ".json") *)

(* Execute the game engine. *)
let () = main ()
