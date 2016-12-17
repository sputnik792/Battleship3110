open Board
open Ship
open Player
open String
open Ai

let zero_grid = Array.make_matrix 10 10 0

(* Kevin's notes: delete later
 counter tells the AI when it is time to switch from random mode to hunt mode,
 counter has lower priority than sunk_ship, which immediately forces AI to switch
 strategy
gflag holds the value that indicates what mode the AI is currently running in*)
let counter = 0
let player_grid = initialize_board 10 10
let player_actual = initialize_board 10 10
let ai_grid = initialize_board 10 10
let sunk_ship = false
let gflag = 0

let min_len = 2
let max_len = 5

(* A [setup] is the combination of a player's actual board and the markup board
 *w ith which the player keeps track of the opponent. *)
type setup = {actual: Board.t; markup: Board.t}

(* A game [state] is the combination of two players' [setup]s and a variable that
 * denotes whose turn it is. If [turn] is true then it's human's turn, otherwise
 * it's AI's turn *)
type state = {human_setup: setup; ai_setup: setup; turn: bool;
                human: Player.t; ai: Player.t}

(* This exception is raised during gameplay. *)
exception Illegal

(*type ai_record = {prev_coord_guess: int*int; counter_tup: int*bool;
       ship_list: Ship.t list; min_number: int; max_number: int;
       game_grid: tile array array; attack_flag: bool; hit_targets: ((int*int)*bool) list;
       main_axis: (int*int) list; axis_dir: int; firstflag: bool } *)

(* Check that this ship does not intersect with previous ships *)
let valid_loc ship =
  let rec check_coords parts_coords =
    (match parts_coords with
    | [] -> true
    | h::t -> let ship_at_coord = get_ship_at_coord (fst h) (snd h) player_actual in
      match ship_at_coord with
      | None -> check_coords t
      | Some s -> false) in
    get_coords ship |> check_coords

let rec get_integer cmd =
  try
    print_string cmd; read_int ()
  with
  | int_of_string -> print_endline "That is not an integer, please try again.";
      get_integer cmd

let rec get_x_y xy =
  let xy_num = get_integer ("Choose the "^xy^" coordinate (0-9)\n") in
  if (xy_num < 0 || xy_num > 9)
    then (
      print_endline "The number is not within range, please enter another number.";
      get_x_y xy
    )
  else xy_num

(* Player sets coordinates for the ship *)
let rec ship_coord name length hv =
  let x = get_x_y "x" in
  let y = get_x_y "y" in
  (* check if there's sufficient space in the board *)
  if (hv = 0)
    then (
      if (y + length <= 10)
        then let ship = new_ship name x y 1 length in
        if valid_loc ship then
          let () = Board.update_ship x y 1 length in ship
        else (print_endline
          "This space cannot be occupied, choose again"; ship_coord name length hv)
      else (print_endline "This ship would be out of the board, please choose another location.";
      ship_coord name length hv)
    )
  else (
    if (x + length <= 10)
      then (
        let ship = new_ship name x y length 1 in
        if valid_loc ship then
          let () = Board.update_ship x y length 1 in ship
        else (print_endline
          "This space cannot be occupied, choose again"; ship_coord name length hv)
      )
    else (print_endline "This ship would be out of the board, please choose another location.";
    ship_coord name length hv)
  )

(* Player sets the horizontal/vertical orientation of a ship *)
let rec ship_hv name length =
  let () = print_endline ("Place your "^name^". It is "^(string_of_int length)^
    " units long.\nWould you like it to be horizontal or vertical?\nType 0 for horizontal, 1 for vertical.") in
  let hv = get_integer "" in if hv <> 0 && hv <> 1 then (print_endline "Incorrect input";
  ship_hv name length)
  else ship_coord name length hv

(* [init ()] starts the game. The game takes user inputs such as whether a player
 * is a human or an AI; if player, one must specify the name; if AI, one must specify
 * the complexity of the AI. Then, each player initializes the board by placing their
 * ships on each board. *)
let init () =
  let () = print_endline "Welcome to Battleship!
  First, you will place your ships." in
  let player_markup = initialize_board 10 10 in
  let () = Board.empty_board () in
  let carrier = ship_hv "Carrier" 5 in
  let player_actual = Board.add_ship carrier player_actual in
  let player_markup = Board.add_ship carrier player_markup in
  let battleship = ship_hv "Battleship" 4 in
  let player_actual = Board.add_ship battleship player_actual in
  let player_markup = Board.add_ship battleship player_markup in
  let cruiser = ship_hv "Cruiser" 3 in
  let player_actual = Board.add_ship cruiser player_actual in
  let player_markup = Board.add_ship cruiser player_markup in
  let submarine = ship_hv "Submarine" 3 in
  let player_actual = Board.add_ship submarine player_actual in
  let player_markup = Board.add_ship submarine player_markup in
  let destroyer = ship_hv "Destroyer" 2 in
  let player_actual = Board.add_ship destroyer player_actual in
  let player_markup = Board.add_ship destroyer player_markup in
  let h = Player.add_ship [carrier; battleship; cruiser;
  submarine; destroyer] in
(*   let a = AI player in *)
  let ai_actual = initialize_board 10 10 in
  let ai_markup = initialize_board 10 10 in
  let randval = Random.int 3 in

  let carrier = if randval == 0 then Ship.new_ship "Carrier" 1 1 5 1 else
      if randval == 1 then Ship.new_ship "Carrier" 0 4 5 1 else
      Ship.new_ship "Carrier" 0 0 5 1 in
  let ai_actual = Board.add_ship carrier ai_actual in
  let ai_markup = Board.add_ship carrier ai_markup in
  let battleship = if randval == 0 then Ship.new_ship "Battleship" 2 3 1 4
      else if randval == 1 then Ship.new_ship "Battleship" 6 1 1 4
      else Ship.new_ship "Battleship" 2 9 5 1 in
  let ai_actual = Board.add_ship battleship ai_actual in
  let ai_markup = Board.add_ship battleship ai_markup in
  let cruiser = if randval == 0 then Ship.new_ship "Cruiser" 7 2 3 1 else
      if randval == 1 then Ship.new_ship "Cruiser" 3 7 1 3  else
      Ship.new_ship "Cruiser"  6 7 3 1 in
  let ai_actual = Board.add_ship cruiser ai_actual in
  let ai_markup = Board.add_ship cruiser ai_markup in
  let submarine = if randval == 0 then Ship.new_ship "Submarine" 6 6 3 1 else
      if randval == 1 then Ship.new_ship "Submarine" 4 6 3 1 else
      Ship.new_ship "Submarine" 8 2 1 3 in
  let ai_actual = Board.add_ship submarine ai_actual in
  let ai_markup = Board.add_ship submarine ai_markup in
  let destroyer = if randval == 0 then Ship.new_ship "Destroyer" 1 8 1 2 else
      if randval == 1 then Ship.new_ship "Destroyer" 8 6 2 1 else
      Ship.new_ship "Destroyer" 1 6 1 2 in
  let ai_actual = Board.add_ship destroyer ai_actual in
  let ai_markup = Board.add_ship destroyer ai_markup in
  let h_ai = Player.add_ship [carrier; battleship; cruiser;
  submarine; destroyer] in


  let splitt = snd (List.split h_ai.ships) in

  let ai_rec = {prev_coord_guess = (0,0); counter_tup = (0, false);
       ship_list = splitt ; min_number = 2; max_number = 5;
       game_grid = player_actual ; attack_flag = true;
       hit_targets = []; main_axis = [] ; axis_dir = 0; firstflag = true } in

  (({human_setup = {actual = player_actual; markup = ai_markup};
    ai_setup = {actual = ai_actual; markup = player_markup}; turn = true;
    human = h; ai = h_ai}), ai_rec)

(*in case of extra helper functions place here*)

let rec cycle_change target_lst acclst =
  match target_lst with
  | [] -> List.rev acclst
  | hd::tl -> let next = ((fst hd), false) in
      cycle_change tl (next::acclst)

let rec strip_lst target_lst acc =
  match target_lst with
  | [] -> acc
  | hd::tl -> let next = if snd hd == true then (fst hd)::acc
else acc in
  strip_lst tl next

(*check how many targets are marked/still marked true*)
let rec check_targets lst accval =
  match lst with
  | [] -> accval
  | hd::tl -> let next = if (snd hd) == false then accval else accval + 1 in
  check_targets tl next

(*cycle*)
let rec cycle_thru target_lst grid flag =
  match target_lst with
  | [] -> flag
  | hd::tl -> if (snd hd) then let tup = fst hd in
  let nxtflag = if (grid.(fst tup).(snd tup)).status == true then true else
  false in cycle_thru tl grid (nxtflag&&flag)
else
  cycle_thru tl grid (false&&flag)

(*check_is_target checks if the coord for the current turn is a hit*)
let check_is_target grid coord =
  if (grid.(fst coord).(snd coord)).ship <> None then
        true else false


let rec iterate_list lst count ccoord acc =
  match lst with
  | [] -> List.rev acc
  | hd::tl -> if hd.coord <> ccoord then
iterate_list tl (count + 1) ccoord (List.rev (hd::acc)) else
let temp = acc@([{coord = ccoord; part_sunken = true}]) in
  temp@tl
(*check if the prev coord was a hit, and if so, checks if
the hit caused a ship to sink,
and returns a new ship_lst based on that information*)
let rec check_sunk coord_lst acc coord =
  match coord_lst with
  | [] -> acc
  | hd::tl -> let curr_string = hd.name in
  let temp = iterate_list (hd.parts) 0 coord [] in
  let nextacc = [{parts = temp; name = curr_string}]@acc in
  check_sunk tl nextacc coord


(* [do' m s] is the result of altering [s] with a the move [m]. *)
let rec do' c st ai_rec =
  let (player_setup,opp_setup) =
    if st.turn then (st.human_setup, st.ai_setup) else (st.ai_setup, st.human_setup) in
  if (String.length c) = 0 then raise Illegal
  else (
    let cmd = (Str.split (Str.regexp " ")
      (String.lowercase_ascii (String.trim c))) in
    let keyword = List.nth cmd 0 in
    if (String.equal keyword "shoot" && List.length(cmd)=3)
      then (
        (* command: shoot x y *)
        (* check in the board *)
        try
          let x = int_of_string (List.nth cmd 1) in
          let y = int_of_string (List.nth cmd 2) in
          let grid_to_check = (if st.turn then st.human_setup.markup else
                                st.ai_setup.markup) in
          if (check_guessed x y grid_to_check = false)
            then (
              let ship_at_coord = get_ship_at_coord x y grid_to_check in
              (
                match ship_at_coord with
                | None -> print_endline "It's a miss.";
                Board.update_shot y x false st.turn;
                  (
                    if st.turn then {
                      st with turn = (not st.turn);
                      human_setup = {
                        actual = st.human_setup.actual;
                        markup = update_guessed st.human_setup.markup x y true
                      }
                    }
                    else {
                      st with turn = (not st.turn);
                      ai_setup = {
                        actual = st.ai_setup.actual;
                        markup = update_guessed st.ai_setup.markup x y true
                      }
                    }
                  )
                | Some s -> (
                  print_endline "It's a hit!";
                  Board.update_shot y x true st.turn;
                    (***** RUDIMENTARY IMPLEMENTATION *****)
                    (* Apply damage to opponent's ship list *)
                    (* Mark hit on the opponent's actual board and player's markup board *)
                    (* let () = if is_sunken s then
                      print_endline (s.name^" has sunken!") else () in *)
                    (* player_setup.markup.(x).(y).status<-true;
                    opp_setup.markup.(x).(y).status<-true; *)
                    let updated_state =
                    (
                      if st.turn then
                        {st with turn = (not st.turn);
                          human_setup = {
                            actual = st.human_setup.actual;
                            markup = update_guessed st.human_setup.markup x y true
                          };
                          ai_setup = {
                            actual = update_guessed st.ai_setup.actual x y true;
                            markup = st.ai_setup.markup
                          };
                          ai = ship_shot st.ai x y (List.assoc s.name st.ai.ships)
                        }
                      else
                        {
                          st with turn = (not st.turn);
                          ai_setup = {
                            actual = st.ai_setup.actual;
                            markup = update_guessed st.ai_setup.markup x y true
                          };
                          human_setup = {
                            actual = update_guessed st.human_setup.actual x y true;
                            markup = st.human_setup.markup
                          };
                          human = ship_shot st.human x y (List.assoc s.name st.human.ships)
                        }
                    ) in
                    if is_sunken (List.assoc s.name updated_state.human.ships) then
                      (print_endline (s.name^" has sunken!"); updated_state)
                    else updated_state
                  )
              )
            )
          else (print_endline "This tile has been guessed before."; st)
        with
        | int_of_string -> (print_endline "Not a valid input, please try again.";
            repl st ai_rec; st)
      )
    else if keyword = "check" && List.length(cmd) = 3 && List.nth cmd 2 = "board"
      then (
        (* command: check (my/tracker) board *)
        (* print the appropriate board *)
        if List.nth cmd 1 = "my" then
          (Board.print_board Mine player_setup.actual; st)
        else if List.nth cmd 1 = "tracker" then
          (Board.print_board Opp player_setup.markup; st)
        else raise Illegal
      )
    else raise Illegal
  )

(*let rec make_coord_lst_lst acclst ship_lst =
  match ship_lst with
  | [] -> acclst
  | hd::tl -> let next = Ship.get_coords hd in
      make_coord_lst_lst (next::acclst) tl *)
(* [repl] implements the REPL-loop that prompts for player inputs and makes fitting
 * changes to the game state. *)
and repl state ai_rec =
  try(
    if not (has_lost state.human) && not (has_lost state.ai) then (
      if state.turn then (
        print_string ("Enter your command below. Valid moves are the following:\n"^
                  "  \"shoot (x) (y)\" guesses the tile at (x,y).\n"^
                  "  \"check my board\" displays the status of the player's own board.\n"^
                  "  \"check tracker board\" displays the status of the player's tracker board.\n"^
                  "  \"quit\" terminates the game.\n");
        let move = String.lowercase_ascii (read_line ()) in
        if move = "quit" then print_endline "Thank you for playing!"
        else (
          let next_state = do' move state ai_rec in
          repl next_state ai_rec
        )
      )
      else (
        print_string ("It's computer's turn.\n Computer does: ");

      let return_rec = Ai.game_turn ai_rec in

      let curr_grid = return_rec.game_grid in
      let curr_min = return_rec.min_number in
      let curr_count_tup = return_rec.counter_tup in
      let curr_dflag = return_rec.attack_flag in
      let curr_ship_lst = return_rec.ship_list in
      let curr_flag2e = return_rec.firstflag in
      let curr_axis_dir = return_rec.axis_dir in
      let ai_shot = return_rec.prev_coord_guess in


      let flag = if check_is_target curr_grid ai_shot then true else false in

      let () = if flag then
      (curr_grid.(fst ai_shot).(snd ai_shot)).status <- true else
      (curr_grid.(fst ai_shot).(snd ai_shot)).status <- false in


      let prev_maj_axis = ai_rec.main_axis in
      let new_axis = if flag then (List.rev (ai_shot::(List.rev prev_maj_axis)))
             else prev_maj_axis in

   (*   let coord_lst_lst = make_coord_lst_lst [] curr_ship_lst in *)
      let next_ship_list =
        check_sunk curr_ship_lst [] ai_shot in

      let curr_target_lst = ai_rec.hit_targets in
      let semi_target_lst = if flag then
          (ai_shot, true)::curr_target_lst else curr_target_lst
        in
      let dummy = if check_targets semi_target_lst 0 == 1 then true
       else false in

      let flag2e = if dummy && (curr_dflag) then true else false in
      (*minval update*)
      let new_min = if (Ship.is_sunken (List.nth next_ship_list 4)) then curr_min + 1
        else curr_min in
      let new_minAB = if (Ship.is_sunken (List.nth next_ship_list 4))&&
      (Ship.is_sunken (List.nth next_ship_list 3))&&(Ship.is_sunken
        (List.nth next_ship_list 2))
        then new_min + 1 else new_min in
      let new_minBC = if (Ship.is_sunken (List.nth next_ship_list 4))&&
      (Ship.is_sunken (List.nth next_ship_list 3))&&
      (Ship.is_sunken (List.nth next_ship_list 2))&&
      (Ship.is_sunken (List.nth next_ship_list 1)) then new_minAB + 1 else new_minAB in
      let last_newmin = if (Ship.is_sunken (List.nth next_ship_list 4))&&
      (Ship.is_sunken (List.nth next_ship_list 3))&&
      (Ship.is_sunken (List.nth next_ship_list 2))&&
      (Ship.is_sunken (List.nth next_ship_list 1))&&
      (Ship.is_sunken (List.nth next_ship_list 0))then
      new_minBC + 1 else new_minBC in

      let next_target_lst = if cycle_thru semi_target_lst curr_grid true then
      cycle_change semi_target_lst [] else semi_target_lst in

      let new_att_flag = if List.length (strip_lst next_target_lst []) == 0 then
      true else false in

      let copy_grid = Array.copy curr_grid in

    let new_ai_rec = {prev_coord_guess = ai_shot; counter_tup = curr_count_tup;
    ship_list =  next_ship_list; min_number = last_newmin; max_number = 5 ;
    game_grid = copy_grid ; attack_flag = new_att_flag;
  hit_targets= next_target_lst; main_axis = new_axis;
  axis_dir = curr_axis_dir; firstflag = flag2e } in

          let () =
      (curr_grid.(fst ai_shot).(snd ai_shot)).status <- false in
      let x_c = string_of_int (fst ai_shot) in
      let y_c = string_of_int (snd ai_shot) in
      let next_state = do' ("shoot "^x_c^" "^y_c) state ai_rec in
              print_endline ("shoot "^x_c^" "^y_c);

      (*changes to ai_record here*)
      repl next_state new_ai_rec
      )
    )
    else (
      if (has_lost state.human) then (print_endline "You lost!")
      else (print_endline "You won!")
    )
  ) with
  | Illegal -> (print_endline "That is not a valid move.\n"; repl state ai_rec)
