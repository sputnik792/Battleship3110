open Player
open Ship
open Board

type ai_record = {prev_coord_guess: int*int; counter_tup: int*bool;
  ship_list: Ship.t list; min_number: int; max_number: int;
  game_grid: tile array array ; attack_flag: bool;
  hit_targets: ((int*int)*bool) list; main_axis: (int*int) list;
  axis_dir: int; firstflag: bool }

type coordinate = int * int (* coordinates of a tile *)

let create_tup x y = if (y + 1) > 9 then (x + 1, 0) else (x, y + 1)

(*add the values of matrices together*)
let rec matrix_add accmat otmat xacc yacc length =
  if xacc > 9 then accmat else
    if (((xacc + yacc + 1) mod length) <> 0) then
      let tup = create_tup xacc yacc in
      matrix_add accmat otmat (fst tup) (snd tup) length
    else
    let elt1 = accmat.(xacc).(yacc) in
    let elt2 = otmat.(xacc).(yacc) in
    let () = accmat.(xacc).(yacc) <- (elt1 + elt2) in
      let tup = create_tup xacc yacc in
      matrix_add accmat otmat (fst tup) (snd tup) length

(*check_valid checks to see if the ship is able to fit into the board in the specified space.
if it can, it returns a one to be added to the value, if not, then it returns a 0*)
let rec iterate_check_valid coord_lst game_grid =
  match coord_lst with
  | [] -> 1
  | hd::tl -> let xc = fst hd in
        let yc = snd hd in
    if (game_grid.(xc).(yc)).status == true then 0
    else iterate_check_valid tl game_grid

(*cycles through each orientation of the ship for a given coordinate*)
let rec check_valid game_grid coord_lst_lst counter =
  match coord_lst_lst with
  | [] -> counter
  | hd::tl -> let inc = iterate_check_valid hd game_grid in
    check_valid game_grid tl (counter + inc)

(*returns a coord list to be added to a list of lists*)
let rec stretch_lstx lst yacc xval count length =
  if count > length then List.rev lst else
    let app_lst = (xval, yacc)::lst in
  stretch_lstx app_lst yacc (xval + 1) (count + 1) length

(*returns a coord list to be added to a list of lists
stretch starts from the starting tile and extends out the whole
length of the ship, then returns to make_lst*)
let rec stretch_lsty lst xacc yval count length =
  if count > length then (List.rev lst) else
    let app_lst = (xacc, yval)::lst in
  stretch_lsty app_lst xacc (yval + 1) (count + 1) length

let rec make_lst_vert bound yacc start length acc counter =
  if counter <= bound then
    let new_lst = (stretch_lstx [] yacc start 1 length)::acc in
    make_lst_vert bound yacc (start + 1) length new_lst (counter + 1)
  else acc

(*make_lst cycles through each possible orientation
adding a list to a (list of lists*)
let rec make_lst_horiz bound xacc start length acc counter  =
  if counter <= bound then
    let new_lst = (stretch_lsty [] xacc start 1 length)::acc in
    make_lst_horiz bound xacc (start + 1) length new_lst (counter + 1)
  else acc

 (*adding ships that are orientated vertically*)
let add_vert game_grid prob_grid xacc yacc length =
  if (game_grid.(xacc).(yacc)).status then [] else
    let up = (9 - (length - 1)) in
    let down =  (length - 1) in
    let bound =
      if xacc > up then 10 - xacc else if (xacc < down) then xacc + 1
        else length in
    let init = if ((xacc + 1) >= length) then (xacc + 1 - length)
          else 0 in
    let lst = make_lst_vert bound yacc init length [] 1 in
    lst

 (*adding ships that are orientated horizontally*)
let add_horiz game_grid prob_grid xacc yacc length =
  if game_grid.(xacc).(yacc).status then [] else
    let up = (9 - (length - 1)) in
    let down =  (length - 1) in
    let bound =
      if yacc > up then 10 - yacc else if yacc < down then yacc + 1
        else length in
    let init = if yacc > up then (yacc + 1 - length) else if
      yacc < down then 0 else (yacc + 1 - length) in
    let lst = make_lst_horiz bound xacc init length [] 1 in
    lst
                    (*bound is how many lists belong to one tile *)

(*iterate_grid will iterate through each required tile, it will pass a coord_lst_lst into
check_valid, which will then use iterate_check_valid to determine whether or not to
increment the corresponding tile value in the prob_grid*)
(*length is how long the ship in question is*)
let rec iterate_grid xacc yacc (game_grid : Board.tile array array) prob_grid length =
    (*this is a safeguard that determines whether this tile is part of the
                    equivalence class modulus (ship length) *)
    if (xacc > 9) then prob_grid else
      if (((xacc + yacc + 1) mod length) <> 0) then
      let tup = create_tup xacc yacc in
      iterate_grid (fst tup) (snd tup) game_grid prob_grid length
      else
      let cd_lst_lst = add_vert game_grid prob_grid xacc yacc length in
      let cd_lst_lst2 = add_horiz game_grid prob_grid xacc yacc length in
      let coord_lst_lst = ((cd_lst_lst)@(cd_lst_lst2))
          in
      (*new_val stores the probability value of the tile, which is added into a new prob_grid*)
      let new_val = check_valid (game_grid) coord_lst_lst 0 in

      let () = prob_grid.(xacc).(yacc) <- new_val in
        let tup = create_tup xacc yacc in
      iterate_grid (fst tup) (snd tup) game_grid prob_grid length


(*form_grid adds the previous grid values to each value in the new grid, using
matrix addition*)
let form_grid_5 prev_grid length game_grid =
  let zerogrid = Array.make_matrix 10 10 0 in
  let new_grid = iterate_grid 0 0 game_grid zerogrid 5 in
  matrix_add new_grid prev_grid 0 0 length

let form_grid_4 prev_grid length game_grid =
  let zerogrid = Array.make_matrix 10 10 0 in
  let new_grid = iterate_grid 0 0 game_grid zerogrid 4 in
  matrix_add new_grid prev_grid 0 0 length

(*mistake: actually a second length 3 ship, not 4*)
let form_grid_3b prev_grid length game_grid =
  let zerogrid = Array.make_matrix 10 10 0 in
  let new_grid = iterate_grid 0 0 game_grid zerogrid 3 in
  matrix_add new_grid prev_grid 0 0 length

let form_grid_3 prev_grid length game_grid =
  let zerogrid = Array.make_matrix 10 10 0 in
  let new_grid = iterate_grid 0 0 game_grid zerogrid 3 in
  matrix_add new_grid prev_grid 0 0 length

let form_grid_2 prev_grid length game_grid =
  let zerogrid = Array.make_matrix 10 10 0 in
  let new_grid = iterate_grid 0 0 game_grid zerogrid 2 in
  matrix_add new_grid prev_grid 0 0 length

(* [guess_coord] randomly generates a coordinate as a guess. *)
let rec guess_coord game_grid =
  let x_val = Random.int 10 in
  let y_val = Random.int 10 in
  if game_grid.(x_val).(y_val).status then
    guess_coord game_grid
  else (x_val, y_val)

(*forces the list to keep only the largest values, and any other values tied with it
returns coord_tup list*)
let rec force_max lst max acc =
  match lst with
  | [] -> acc
  | hd::tl -> if (fst hd) == max then force_max tl max (hd::acc)
    else if (fst hd) > max then force_max tl (fst hd) (hd::[])
    else force_max tl max acc

(*select from the coordinates that are fed into the list, using random number
generator*)
let choose_from coord_tup_lst =
  let value = Random.int (List.length coord_tup_lst) in
  List.nth coord_tup_lst value

let rec cycle_vert xacc vert_lst length flag counter =
  if counter == length then flag else
    let nxtflag = if (List.nth vert_lst xacc) == 75 then flag&&false else
      flag&&true in
    cycle_vert (xacc + 1) vert_lst length flag (counter + 1)

let rec cycle_horiz yacc horiz_lst length flag counter =
  if counter == length then flag else
    let nxtflag = if (List.nth horiz_lst yacc) == 75 then flag&&false else
      flag&&true in
    cycle_horiz (yacc + 1) horiz_lst length flag (counter + 1)

let rec apply_vert value yacc vert_lst length count bound tileval =
  if count > bound then tileval else
  let nextval = if (cycle_vert value vert_lst length true 0) then tileval + 1
      else tileval in
    apply_vert (value + 1) yacc vert_lst length (count + 1) bound nextval

let rec apply_horiz xacc value horiz_lst length count bound tileval =
  if count > bound then tileval else
    let nextval = if (cycle_horiz value horiz_lst length true 0) then tileval + 1
      else tileval in
    apply_horiz xacc (value + 1) horiz_lst length (count + 1) bound nextval

let make_vert_lsts xacc yacc count bound accval acclst vert_lst minval =
  ((apply_vert accval yacc vert_lst minval count bound 0), (xacc, yacc))::acclst

let make_horiz_lsts xacc yacc count bound accval acclst horiz_lst minval =
  ((apply_horiz xacc accval horiz_lst minval count bound 0), (xacc, yacc))::acclst

(*flag = true -> horizontal*)
(*this function creates either the whole row or the whole column,
depending on the flag*)
let rec strip_out prob_grid xval yval flag acc accval =
  if accval > 9 then acc else
  if flag then
  let curr = prob_grid.(xval).(accval) in
  strip_out prob_grid xval yval flag (curr::acc) (accval + 1)
  else let curr = prob_grid.(accval).(yval) in
  strip_out prob_grid xval yval flag (curr::acc) (accval + 1)

(*flag = true -> going towards the increasing positive direction*)
let rec strafe_vert xacc yacc accval flag vert_lst prob_grid minval acclst =
  if flag then
    let next = accval + 1 in
    if next > 9 then strafe_vert xacc yacc xacc false vert_lst prob_grid minval acclst
          (*tries the other side of the target*)
    else
      if (List.nth vert_lst next) == 75 then
      strafe_vert xacc yacc next false vert_lst prob_grid minval [(0, (next, yacc))]
      else
        let rec_bound_tup = if next < (minval - 1) then ((next + 1), 0) else
              if next > (10 - minval) then ((10 - next), next - (minval - 1)) else
              (minval, (minval - 1)) in
              let vlst = make_vert_lsts next yacc 1
              (fst rec_bound_tup) (snd rec_bound_tup) [] vert_lst minval
            in
            strafe_vert xacc yacc yacc false vert_lst prob_grid minval vlst
  else
    let next = accval - 1 in
    if next < 0 then acclst else
    if (List.nth vert_lst next) == 75 then
    (0, (next, yacc))::acclst
    else
        let rec_bound_tup = if next < (minval - 1) then ((next + 1), 0) else
              if next > (10 - minval) then ((10 - next), next - (minval - 1)) else
              (minval, (minval - 1)) in
              let vlst = make_vert_lsts next yacc 1
              (fst rec_bound_tup) (snd rec_bound_tup) [] vert_lst  minval
          in
          vlst@acclst

(*called by examine_horiz *)
let rec strafe_horiz xacc yacc accval flag horiz_lst prob_grid minval acclst =
  if flag then
    let next = accval + 1 in
    if next > 9 then strafe_horiz xacc yacc yacc false horiz_lst prob_grid minval acclst
          (*tries the other side of the target*)
    else
      if (List.nth horiz_lst next) == 75 then
      strafe_horiz xacc yacc next false horiz_lst prob_grid minval [(0, (xacc, next))]
      else
        let rec_bound_tup = if next < (minval - 1) then ((next + 1), 0) else
              if next > (10 - minval) then ((10 - next), next - (minval - 1)) else
              (minval, (minval - 1)) in
              let vlst = make_horiz_lsts xacc next 1
              (fst rec_bound_tup) (snd rec_bound_tup) [] horiz_lst minval
              (*snd refers to the initial tile the funtion starts on *)
              (*vlst is the list containing tuples of prob value and coordinates*)
            in
            strafe_horiz xacc yacc yacc false horiz_lst prob_grid minval vlst
  else
    let next = accval - 1 in
    if next < 0 then acclst else
    if (List.nth horiz_lst next) == 75 then
    (0, (xacc, next))::acclst
    else
        let rec_bound_tup = if next < (minval - 1) then ((next + 1), 0) else
              if next > (10 - minval) then ((10 - next), next - (minval - 1)) else
              (minval, (minval - 1)) in
              let vlst = make_horiz_lsts xacc next 1
              (fst rec_bound_tup) (snd rec_bound_tup) [] horiz_lst minval
          in
          vlst@acclst


(*examine functions both add the highest values in the rows/columns
to a list that *)
let examine_vert prob_grid xacc yacc minval =
  let vert_lst = strip_out prob_grid xacc yacc false [] 0 in
    (*this is the list used by examine in order to execute probability
      grid operations on*)

  let cc = strafe_vert xacc yacc xacc true vert_lst prob_grid minval [] in
  force_max cc (-100) []

let examine_horiz prob_grid xacc yacc minval =
  let horiz_lst = strip_out prob_grid xacc yacc true [] 0 in
  let cc = strafe_horiz xacc yacc yacc true horiz_lst prob_grid minval [] in
  force_max cc (-100) []

let rec check_number coordlst acc =
  match coordlst with
  | [] -> acc
  | hd::tl -> if (snd hd) then let next = hd::acc in
    check_number tl next else
      check_number tl acc


(*----------------*)

let rec get_end lst acc =
  match lst with
  | [] -> acc
  | hd::tl -> get_end tl hd

(*axisdir = 1 means horiz, 2 means vert*)
(*assume maj_alist is sorted already *)
let det_ends_lst maj_alist axisdir game_grid =
  let end1 = List.hd maj_alist in
    let end2 = get_end maj_alist (0,0) in
  let lst = if axisdir == 1 then
    let c1 = if (snd end1) - 1 < 0 then [] else
          if (game_grid.(fst end1).((snd end1) - 1)).status == true then [] else
          [((fst end1), (snd end1) - 1)] in
    let c2 = if (snd end2) + 1 > 9 then [] else
          if (game_grid.(fst end1).((snd end1) + 1)).status == true then [] else
          [((fst end2), (snd end2) + 1)] in
    c1@c2
  else
    let c1 = if (fst end1) - 1 < 0 then [] else
          if (game_grid.((fst end1) - 1).(snd end1)).status == true then [] else
          [ (((fst end1) - 1), (snd end1)) ] in
    let c2 = if (fst end2) + 1 > 9 then [] else
          if (game_grid.((fst end1) + 1).(snd end1)).status == true then [] else
          [ (((fst end2) + 1), (snd end2)) ] in
    c1@c2 in
  lst


let rec expand_horiz axis_lst acclst game_grid =
  match axis_lst with
  | [] -> acclst
  | hd::tl -> let xcurr = fst hd in
  let ycurr = snd hd in
  let tile1 = if (game_grid.(xcurr).(ycurr - 1)).status then [] else
    (xcurr, (ycurr - 1))::[] in
  let tile2 = if (game_grid.(xcurr).(ycurr + 1)).status then [] else
    (xcurr, (ycurr + 1))::[] in
  expand_horiz tl (tile2@acclst) game_grid

let rec expand_vert axis_lst acclst game_grid =
  match axis_lst with
  | [] -> acclst
  | hd::tl -> let xcurr = fst hd in
  let ycurr = snd hd in
  let tile1 = if game_grid.(xcurr).(ycurr - 1).status then [] else
    ((xcurr - 1), ycurr)::[] in
  let tile2 = if game_grid.(xcurr).(ycurr + 1).status then [] else
    ((xcurr + 1), ycurr)::[] in
  expand_horiz tl (tile2@acclst) game_grid

(*get the tiles on both sides of the major axis*)
let expand_list maj_alist target_lst game_grid axisdir =
  let ext_lst = if axisdir == 1 then expand_horiz maj_alist [] game_grid
  else expand_vert maj_alist [] game_grid in
  ext_lst

let select_target ex_lst =
  let randval = Random.int (List.length ex_lst) in
  List.nth ex_lst randval

(*expansion of guess_nearby*)
let guess_next maj_alist target_lst game_grid axisdir =
  let cap_lst = det_ends_lst maj_alist axisdir game_grid in
  let sel_lst = if (List.length cap_lst) == 0
  then expand_list maj_alist target_lst game_grid axisdir else
  cap_lst in
  ((-10), select_target sel_lst)
  (*random value, not important*)


(*initialize flag in the beginning of the game*)
let guess_nearby game_grid prob_grid target_lst main_axis_lst
        maxval minval flag axisdir =
  let amt_att = check_number target_lst [] in
  if ((List.length amt_att) == 1) && flag  then  (*this is the axisdir safeguard*)
  let ctile = List.hd amt_att in
  let xacc = fst (fst ctile) in
  let yacc = snd (fst ctile) in
    let adjc_lst = ((examine_vert prob_grid xacc yacc minval)@
      (examine_horiz prob_grid xacc yacc minval)) in

    let fine_lst = force_max adjc_lst (-100) [] in
    let refine = choose_from fine_lst in
    (refine, false)  (*(int,coord), bool *)

(*the code that runs if it's not the first time, needs to
first, elongate the search to include non-hit tiles around the major axis, but
still prioritize the ends of the major axis, if the ends aren't successful, then
the AI goes into random mode*)
  else
    let tup = guess_next main_axis_lst target_lst game_grid axisdir in
    (tup, false)

(**)
let rec is_hit xc yc target_lst =
  match target_lst with
  | [] -> false
  | hd::tl -> if (snd hd) == false then is_hit xc yc tl
  else
  let tup = (fst hd) in
   if (xc == fst tup)&&(yc == snd tup) then true
      else is_hit xc yc tl

(*here, we will denote (100) to mean a hit target being considered,
(75) to mean a miss(or already sunken), and (50) to mean not considered
include a list of all coordinates to be considered, or else assign the unconsidered
values a number *)

(*create the grid before Ai enters into destroy mode*)
let rec build_init_grid grid game_grid xacc yacc target_lst =
  if xacc > 9 then grid else
    let tup = create_tup xacc yacc in    (*might need to change some command word names*)
    if (game_grid.(xacc).(yacc)).status then
      let () = grid.(xacc).(yacc) <- 75 in
      build_init_grid grid game_grid (fst tup) (snd tup) target_lst
    else
      if is_hit xacc yacc target_lst then
      let () = grid.(xacc).(yacc) <- 100 in
      build_init_grid grid game_grid (fst tup) (snd tup) target_lst
    else
      let () = grid.(xacc).(yacc) <- 50 in
      build_init_grid grid game_grid (fst tup) (snd tup) target_lst

(*randomized targetting of the tiles with the highest probability in the
modulus class*)
let rec coord_prob prob_grid xacc yacc length acclst maxval =
  if xacc > 9 then acclst else
    if (((xacc + yacc + 1) mod length) <> 0) then
      let tup = create_tup xacc yacc in
      coord_prob prob_grid (fst tup) (snd tup) length acclst maxval
    else
      let curr_val = prob_grid.(xacc).(yacc) in
      if curr_val < maxval then
        let tup = create_tup xacc yacc in
        coord_prob prob_grid (fst tup) (snd tup) length acclst maxval
      else
      let crdtup = (xacc, yacc) in
      if curr_val == maxval then
        let new_lst = crdtup::acclst in
        let tup = create_tup xacc yacc in
      coord_prob prob_grid (fst tup) (snd tup) length new_lst maxval
      else (*if curr_val > maxval*)
        let new_lst = crdtup::[] in
        let tup = create_tup xacc yacc in
        coord_prob prob_grid (fst tup) (snd tup) length new_lst curr_val

let rec guess_hunt game_grid prob_grid length =
  let cd_int_lst = coord_prob prob_grid 0 0 length [] (-100) in
  let r_val = Random.int (List.length cd_int_lst) in
  List.nth cd_int_lst r_val

(*flag = true means it is still hunting, false means it is no longer in
hunt mode, but in destroy mode*)
(*flag2 tells the AI if the attacked tile is the first in a series
flag2 = true means first tile*)
(*consider hunting the main calling program*)
let hunting game_grid prob_grid flag min_val max_val target_lst main_axis_lst
flag2 axisdir =
  if (flag == false) then
  let tup = guess_nearby game_grid prob_grid target_lst main_axis_lst max_val
  min_val flag2 axisdir  in (*(int,coord), bool *)
  (false, ((snd (fst tup)), snd tup) ) else   (*(bool, (coord, bool))*)
    (true, ((guess_hunt game_grid prob_grid min_val), true))

let det_switch_phase cnt_flag_tup =
  let counter = fst cnt_flag_tup in
  if (snd cnt_flag_tup) then (counter, true)
  else if counter == 7 then (counter, true) else (counter + 1 , false)
(*this counter determines how many turns it takes for the AI to automatically switch
to hunt and destroy mode, provided it hasn't sunk any ships yet*)

(*game_grid is a tile array array
the probability matrix is zeroed out at the start of each ai turn
ship_flist is the player's ship list, which is a list of ship objects
cnt_flag_tup is made up of the counter and the game-phase flag (false = random mode)
dflag, or attack_flag, indicates if the AI is in destroy mode (true)
target_lst is the list of coordinates that have been sunk during destroy-mode, a target
marked as true means that it belongs to a ship that hasn't sunk yet
*)

let game_turn ai_rec =
  let gamegrid = ai_rec.game_grid in
  let cnt_flag_tup = ai_rec.counter_tup in
  let ship_flist = ai_rec.ship_list in
  let min_num = ai_rec.min_number in
  let dflag = ai_rec.attack_flag in
  let target_lst = ai_rec.hit_targets in
  let maj_ax = ai_rec.main_axis in
  let max_num = ai_rec.max_number in
  let flag2e = ai_rec.firstflag in
  let axisdir = ai_rec.axis_dir in

  let zerogrid = Array.make_matrix 10 10 0 in
  let flag_tup = det_switch_phase cnt_flag_tup in

  if (snd flag_tup) == false then let coord = guess_coord gamegrid in

  {prev_coord_guess = coord; counter_tup = flag_tup; ship_list = ship_flist;
  min_number = min_num; max_number = max_num; game_grid = gamegrid; attack_flag = dflag;
  hit_targets = target_lst; main_axis = maj_ax; axis_dir = axisdir; firstflag = true}
          (*note: remember that a hit in the random mode phase of the game needs to be recorded.
        a hit sets the flag in counter_tup to true in 'game' *)
  else

  if (dflag == false) then   (*need to fix this safeguard part later*)
  let newgrid1 = if not (Ship.is_sunken (List.nth ship_flist 4)) then
      form_grid_2 zerogrid min_num gamegrid
    else zerogrid in
  let newgrid2 = if not (Ship.is_sunken (List.nth ship_flist 3)) then
      form_grid_3 newgrid1 min_num gamegrid
    else newgrid1 in
  let newgrid3 = if not (Ship.is_sunken (List.nth ship_flist 2)) then
      form_grid_3b newgrid2 min_num gamegrid
    else newgrid2 in
  let newgrid4 = if not (Ship.is_sunken (List.nth ship_flist 1)) then
      form_grid_4 newgrid3 min_num gamegrid
    else newgrid3 in
  let newgrid5 = if not (Ship.is_sunken (List.nth ship_flist 0)) then
      form_grid_5 newgrid4 min_num gamegrid
    else newgrid4 in

      let fintup = hunting gamegrid newgrid5 dflag min_num max_num target_lst
      maj_ax flag2e axisdir in (*(bool, (coord, bool))*)
                                        (*hunt flag, (coord, flag) *)
      let coord = (fst (snd fintup)) in
      let at_flag = (snd (snd fintup)) in
      let new_flag = fst fintup in

      {prev_coord_guess = coord; counter_tup = flag_tup; ship_list = ship_flist;
      min_number = min_num; max_number = max_num; game_grid = gamegrid; attack_flag = new_flag;
      hit_targets = target_lst; main_axis = maj_ax; axis_dir = axisdir; firstflag = at_flag }


      (*note: if a hit occurs during hunt-mode, set attack_flag to true in 'game'*)
  else
    let status_grid = build_init_grid zerogrid gamegrid 0 0 target_lst in

    let fintup = hunting gamegrid status_grid dflag min_num max_num target_lst maj_ax
      flag2e axisdir in (*(bool, (coord, bool))*)
                                        (*hunt flag, (coord, flag) *)
    let coord = (fst (snd fintup)) in
    let at_flag = (snd (snd fintup)) in
    let new_flag = fst fintup in

      {prev_coord_guess = coord ; counter_tup = flag_tup ; ship_list = ship_flist ;
      min_number = min_num; max_number = max_num ;
      game_grid = gamegrid; attack_flag = new_flag ; hit_targets = target_lst;
      main_axis = maj_ax; axis_dir = axisdir; firstflag = at_flag
      }


(*extra notes: when the AI sends a coord back to game, game will determine if it's a hit, then will
add that coord to the hit_targets list, but only if there's a single hit
game will leave destroy mode only when there are no more targets that are marked hit in the hit_targets list

Game module will dictate which tags to change, including flag_tup, ship_list, min, max, game_grid, attack_falg, hit_targets,
main_axis, and firstflag

main_axis is techincally created when the first hit occurs, but not really used until guess_nearby takes place
set axisdir to 0 at the beginning of the game
 *)





