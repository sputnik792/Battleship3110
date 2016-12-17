open Ship

type t = {
  ships: (string * Ship.t) list
}

(* The gameplay mechanics for the player *)

(* Adds newly added ships into the player's ship list *)
val add_ship : Ship.t list -> t

(* Apply damage on ship *)
val ship_shot : t -> int -> int -> Ship.t -> t

(* Checks if all the ships have sunk *)
val has_lost : t -> bool