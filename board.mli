open Graphics
open Ship

type guessed = bool (* true if this space has been guessed, otherwise false *)

(* Tiles that comprise a board
 * A [tile] without a ship part has None as its [ship] field. *)
type tile = {mutable status: guessed; mutable ship: Ship.t option}

(* Type of a Board is a 2D-array of tiles.
 * Note: the board is mutable! Inspired by Prelim 2 part 2. *)
type t = tile array array

(* Displaymode determines how to print the board value.
 * [Mine] displays the ship positions, guessed positions, and sunken parts.
 * [Opp] displays only the guessed positions, and sunken parts. *)
type displaymode = Mine | Opp

(* Initialize empty board of rectangular size *)
val initialize_board: int -> int -> t

(* Add a ship onto a board and return the new board *)
val add_ship: Ship.t -> t -> t

(* Updates the guessed status of a tile.
 * original_board -> x -> y -> guessed -> updated_board *)
val update_guessed: t -> int -> int -> bool -> t

(* Updates the tile that has been shot to None *)
(* val update_shot: t -> int -> int -> t *)

(* Checks whether the tile (x,y) has been guessed or not *)
val check_guessed: int -> int -> t -> bool

(* Get the ship object at a given coordinates *)
val get_ship_at_coord: int -> int -> t -> Ship.t option

(* Display the board in ascii format, depending on the [displaymode].
 * Heavily inspired by Prelim 2 Part 2 *)
val print_board : displaymode -> t -> unit

(* Draw empty board *)
val empty_board: unit -> unit

(* Draw ship of coordinates x,y and dimensions length, width onto board *)
val update_ship: int -> int -> int -> int -> unit

(* Add shot of coordinates x,y based on whether it hit and which player *)
val update_shot: int -> int -> bool -> bool -> unit
