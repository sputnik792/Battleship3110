open Ship
open Board
open Player
open Game
(* open Ai *)

let () =
  ANSITerminal.(print_string [red]
    "\n\nBattleship\n");
  print_endline ("Welcome to the 3110 Final Project!\n"^
                "Follow the instructions on setting up the game, and then play the game!\n"^
                "Valid moves are the following:\n"^
                "  \"shoot (x) (y)\" guesses the tile at (x,y).\n"^
                "  \"check my board\" displays the status of the player's own board.\n"^
                "  \"check tracker board\" displays the status of the player's tracker board.\n"^
                "  \"quit\" terminates the game.\n");
  let init_state = init () in
  repl (fst init_state) (snd init_state)