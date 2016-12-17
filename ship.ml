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

let rec make_column x y h : part list =
  match h with
  | 0 -> []
  | a ->
    {
      coord = (x, y);
      part_sunken = false;
    }::(make_column (x+1) y (a-1))

let rec make_parts (x: int) (y: int) (h: int) (w: int) =
  match w with
  | 0 -> []
  | n -> (make_column x y h)@(make_parts x (y+1) h (w-1))

let new_ship s_name x y h w =
  let s_parts = make_parts x y h w in
  {
    name = s_name;
    parts = s_parts;
  }

let is_sunken ship =
  let rec sunken_helper parts=
    match parts with
    | [] -> true
    | h::t ->
        if (h.part_sunken = false)
          then false
        else sunken_helper t
  in sunken_helper ship.parts

let part_shot ship c =
  if (List.exists (fun x -> x.coord = c) ship.parts)
    then (
      let new_parts = {coord = c; part_sunken = true}::
        (List.filter (fun x -> x.coord <> c) ship.parts)
      in
      {
        parts = new_parts;
        name = ship.name;
      }
    )
  else raise Invalid_part

let is_part_sunken c ship =
  (List.find (fun x -> x.coord = c) ship.parts).part_sunken

let get_coords ship =
  let rec part_to_coords parts_list =
    match parts_list with
    | [] -> []
    | h::t -> [h.coord]@(part_to_coords t)
  in part_to_coords ship.parts