open Ship

type t = {
  ships: (string * Ship.t) list
}

let add_ship ship_list =
  let rec add_ship_helper lst =
    match lst with
    | [] -> []
    | h::t -> (h.name, h)::(add_ship_helper t)
  in {ships = add_ship_helper ship_list}

let ship_shot player x y ship =
  let damaged_ship = Ship.part_shot ship (x, y) in
  let ship_name = ship.name in
  {ships = (ship_name, damaged_ship)::(List.remove_assoc ship.name player.ships)}

let rec all_sunk ship_list =
  match ship_list with
  | h::t -> (is_sunken (snd h)) && all_sunk t
  | [] -> true

(* Checks if all the ships have sunk *)
let has_lost player =
  all_sunk player.ships