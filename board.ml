open Graphics
open Ship

type guessed = bool (* true if this space has been guessed, otherwise false *)

type tile = {mutable status: guessed; mutable ship: Ship.t option}

type t = tile array array

type displaymode = Mine | Opp

(* Initialize empty board of rectangular size *)
let initialize_board row col =
  let board = Array.make_matrix row col {status=false;ship=None} in
  for i = 0 to row - 1
    do (
      for j = 0 to col - 1 do
        board.(i).(j) <- {status=false;ship=None}
      done
    )
  done;
  board

(* Check whether tile coordinates (x,y) has been guessed *)
let check_guessed x y board =
  board.(x).(y).status

(* Updates the guessed status of a tile.
 * original_board -> x -> y -> guessed -> updated_board *)
let update_guessed orig x y guessed =
  orig.(x).(y).status <- guessed; orig

(* Get the ship at tile with coordinates (x,y) *)
let get_ship_at_coord x y board =
  board.(x).(y).ship

(* Draws an empty board. There's definitely a better way to do this,
   but I can only think of using a for loop. *)
let empty_board () =
  open_graph ""; resize_window 1000 500;
  rgb 200 200 200 |> set_color; fill_rect 500 0 1000 500;
  set_color black;
  moveto 50 0; lineto 50 500; moveto 100 0; lineto 100 500;
  moveto 150 0; lineto 150 500; moveto 200 0; lineto 200 500;
  moveto 250 0; lineto 250 500; moveto 300 0; lineto 300 500;
  moveto 350 0; lineto 350 500; moveto 400 0; lineto 400 500;
  moveto 450 0; lineto 450 500; moveto 500 0; lineto 500 500;
  moveto 550 0; lineto 550 500; moveto 600 0; lineto 600 500;
  moveto 650 0; lineto 650 500; moveto 700 0; lineto 700 500;
  moveto 750 0; lineto 750 500; moveto 800 0; lineto 800 500;
  moveto 850 0; lineto 850 500; moveto 900 0; lineto 900 500;
  moveto 950 0; lineto 950 500; moveto 0 50; lineto 1000 50;
  moveto 0 100; lineto 1000 100; moveto 0 150; lineto 1000 150;
  moveto 0 200; lineto 1000 200; moveto 0 250; lineto 1000 250;
  moveto 0 300; lineto 1000 300; moveto 0 350; lineto 1000 350;
  moveto 0 400; lineto 1000 400; moveto 0 450; lineto 1000 450

(* Draw ships in the GUI. This should only be used in
   the init phase. *)
let update_ship y x length width =
  rgb 92 92 92 |> set_color;
  fill_rect (500 + 50*x) (500 - 50*y - 50*length) (50*width) (50*length)

(* Show guessed tiles in the GUI *)
let update_shot x y hit mine =
  if hit then rgb 180 24 50 |> set_color
  else set_color green;
  if mine then fill_rect (50*x) (450 - 50*y)
  50 50 else
  fill_rect (500 + 50*x) (450 - 50*y)
  50 50

(* Change tiles within the board *)
let add_ship (sh:Ship.t) board =
  let coord_list = List.map (fun (part:Ship.part) -> part.coord) sh.parts in
  let ship (a:int*int) () = board.(fst a).(snd a) <- {status = false; ship = Some sh} in
  List.fold_right ship coord_list (); board

let bg = ANSITerminal.on_black

let style_of_int i =
  let open ANSITerminal in
  match i with
    | 0 -> black
    | 1 -> blue
    | 2 -> green
    | 3 -> cyan
    | 4 -> red
    | 5 -> magenta
    | 6 -> yellow
    | 7 -> white
    | _ -> failwith "Illegal color"

(* blue means it has been guessed but there was no ship,
 * red means it was a hit *)
let display mode {status = gssd; ship = name} row col board =
  let the_ship = get_ship_at_coord row col board in
  let hit =
    match the_ship with
    | None -> false
    | Some sh -> is_part_sunken (row, col) sh
  in
  let (i,str) = match mode, gssd with
    | Mine,true  -> if name = None then (7,".") (* guessed but nothing there *)
                      else (4,"*") (* guessed and there was something (a hit) *)
    | Mine,false -> if name = None then (7,".") else (0,"*")
    | Opp, false -> (7,".")
    | Opp, true  -> if name = None then (0,"*") (* guessed but nothing there *)
                      else (1,"*") (* guessed and there was something *)
  in ANSITerminal.sprintf [bg; style_of_int i] "%s" str

let string_it mode board =
  let row = Array.length board in
  let col = Array.length (board.(0)) in
  let buf = Buffer.create ((row+1)*(col+1)) in
  for i = 0 to row-1 do
    for j = 0 to col-1 do
      Buffer.add_string buf (display mode board.(i).(j) i j board)
    done;
    Buffer.add_string buf "\n"
  done;
  Buffer.contents buf

let print_board mode board =
  board |> string_it mode |> print_string