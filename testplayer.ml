open OUnit2
open Player
open Ship

let part_a = {coord = (0,0); part_sunken = false}
let part_b = {coord = (0,1); part_sunken = false}
let part_c = {coord = (0,2); part_sunken = false}
let part_d = {coord = (1,0); part_sunken = false}
let part_e = {coord = (1,1); part_sunken = false}
let part_f = {coord = (1,2); part_sunken = false}
let part_g = {coord = (1,3); part_sunken = false}

let ship_a = {parts = [part_a]; name = "test ship a"}
let ship_longer = {parts = [part_d; part_e; part_f]; name = "test ship b"}
let ship_longer_2 = {parts = [part_g; part_e; part_f]; name = "test ship c"}
let long_ship_2d = {parts = [part_a; part_b; part_c; part_d; part_e; part_f]; 
  name = "test ship d"}

let ship_list = [ship_a; ship_longer]
let player_1 = add_ship ship_list
let poor_player = {ships = [("test ship a", ship_a)]}

let rec all_ships_exist test model = 
  match test with
  | [] -> true
  | h::t -> if(List.mem (snd h) model) then (all_ships_exist t model) else false

let same_length_and_ships test model =
  (all_ships_exist test model)&&(List.length test = List.length model)

let tests = "test suite" >::: [
  "add 2 ships"  >::
    (fun _ -> assert_equal true 
      (same_length_and_ships (add_ship ship_list).ships ship_list));
  "apply damage on ship"  >::
    (fun _ -> assert_equal true (is_sunken (List.assoc "test ship a" (ship_shot 
      player_1 0 0 ship_a).ships)));
  "has not lost yet" >::
    (fun _ -> assert_equal false (has_lost (ship_shot player_1 0 0 ship_a)));
  "poor player lost"  >::
    (fun _ -> assert_equal true (has_lost (ship_shot poor_player 0 0 ship_a)));
  "has lost" >::
    (fun _ -> assert_equal true 
      (
        has_lost (
          let shot_1 = ship_shot player_1 0 0 (List.assoc "test ship a" 
            player_1.ships) in
          let shot_2 = ship_shot shot_1 1 0 (List.assoc "test ship b" 
            shot_1.ships) in
          let shot_3 = ship_shot shot_2 1 1 (List.assoc "test ship b" 
            shot_2.ships) in
          let shot_4 = ship_shot shot_3 1 2 (List.assoc "test ship b" 
            shot_3.ships) in
          shot_4
        )
      )
    );
]

let _ = run_test_tt_main tests
