open OUnit2
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

let rec all_coord_exist test model = 
  match test with
  | [] -> true
  | h::t -> if(List.mem h model) then all_coord_exist t model else false

let same_length_and_parts test model =
  (all_coord_exist test model)
  &&(List.length test = List.length model)

let tests = "test suite" >::: [
  "create simple ship"  >::
    (fun _ -> assert_equal true 
      (same_length_and_parts (new_ship "test ship a" 0 0 1 1).parts 
        ship_a.parts));
  "create longer ship"  >::
    (fun _ -> assert_equal true 
      (same_length_and_parts (new_ship "test ship a" 1 0 1 3).parts 
        ship_longer.parts));
  "create longer ship #2"  >::
    (fun _ -> assert_equal true 
      (same_length_and_parts (new_ship "test ship a" 1 1 1 3).parts 
        ship_longer_2.parts));
  "create long 2d ship"  >::
    (fun _ -> assert_equal true
    (same_length_and_parts (new_ship "test ship a" 0 0 2 3).parts 
      long_ship_2d.parts));
  "test ship part not sunken"  >::
    (fun _ -> assert_equal
    false (is_part_sunken (0,0) (new_ship "test ship a" 0 0 1 1)));
  "test ship not sunken"  >::
    (fun _ -> assert_equal
    false (is_sunken (new_ship "test ship a" 0 0 1 1)));
  "test ship sunken"  >::
    (fun _ -> assert_equal true
    (part_shot (new_ship "test ship a" 0 0 1 1) (0,0) |> is_sunken));
  "test ship_longer not (completely) sunken"  >::
    (fun _ -> assert_equal false
    (part_shot ship_longer (1,0) |> is_sunken));
]

let _ = run_test_tt_main tests
