type part = {
  coord: int * int;
  part_sunken: bool;
}

(* The type of ship *)
type t = {
  parts: part list;
  name: string;
}

exception Invalid_part

(* takes a name:string, x_coord:int, y_coord:int, height:int, width:int and
 * returns a new ship based on the given arguments. x_coord and y_coord are
 * the coordinates of the bottom-leftmost coordinate position of the ship.
 * 
 * precondition: all integers passed to this function should be >= 0
 *               if any integer is < 0, behaviour is unspecified 
 *               a.k.a don't do it plz 
 *)
val new_ship : string -> int -> int -> int -> int -> t

(* takes a ship object and returns true if the all parts of the ship are
 * sunken, returns false otherwise *)
val is_sunken : t -> bool

(* takes a ship object and a coordinate of the part that has been shot,
 * then returns the new ship with updated parts that has been shot
 * raises: Invalid_part exception if part is not found *)
val part_shot : t -> int * int -> t

(* takes coordinates (x, y) and a ship object, then returns whether the ship
 * part at the given coordinate is sunken or not (for AI purposes?) *)
val is_part_sunken : int * int -> t -> bool

(* Also for AI purposes; return the coordinates of ONE given ship *)
val get_coords: t -> (int*int) list