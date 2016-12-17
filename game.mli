open Board
open Ship
open Ai
open Player
open String

(* A [setup] is the combination of a player's actual board and the markup board
 *w ith which the player keeps track of the opponent. *)
type setup = {actual: Board.t; markup: Board.t}

(* A game [state] is the combination of two players' [setup]s and a variable that
 * denotes whose turn it is. If [turn] is true then it's human's turn, otherwise
 * it's AI's turn *)
type state = {human_setup: setup; ai_setup: setup; turn: bool;
                human: Player.t; ai: Player.t}


 (* type ai_record = {prev_coord_guess: int*int; counter_tup: int*bool;
  ship_list: Ship.t list; min_number: int; max_number: int;
  game_grid: tile array array ; attack_flag: bool;
  hit_targets: ((int*int)*bool) list; main_axis: (int*int) list;
  axis_dir: int; firstflag: bool } *)

(* This exception is raised during gameplay. *)
exception Illegal

(* [init ()] starts the game. The game takes user inputs such as whether a player
 * is a human or an AI; if player, one must specify the name; if AI, one must specify
 * the complexity of the AI. Then, each player initializes the board by placing their
 * ships on each board. *)
val init : unit -> state* ai_record

(* [repl] implements the REPL-loop that prompts for player inputs and makes fitting
 * changes to the game state. *)
val repl : state -> Ai.ai_record -> unit

(* [do' m s] is the result of altering [s] with a the move [m]. *)
val do' : string -> state -> Ai.ai_record -> state