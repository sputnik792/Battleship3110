open OUnit2
open Graphics
open Board

let board = initialize_board 10 10
let part_a = Ship.({coord=(0,0); part_sunken=false})
let part_b = Ship.({coord=(0,1); part_sunken=false})
let part_c = Ship.({coord=(1,0); part_sunken=true})
let part_d = Ship.({coord=(2,0); part_sunken=true})
let part_e = Ship.({coord=(3,0); part_sunken=false})
let ship_a = Ship.({parts=[part_a]; name="small ship"})
let ship_b = Ship.({parts=[part_a; part_b]; name="larger ship"})
let ship_c = Ship.({parts=[part_a; part_c; part_d; part_e]; name="big ship"})

let tests = "test suite" >::: [
(*   "0x0"  >::
    (fun _ -> assert_equal
    0 (initialize_board 0 0 |> Array.length));
  "1x1"  >::
    (fun _ -> assert_equal
    1 (initialize_board 1 1 |> Array.length));
  "2x2"  >::
    (fun _ -> assert_equal
    2 (initialize_board 2 2 |> Array.length));
  "2x2 ship"  >::
    (fun _ -> assert_equal
    None (initialize_board 2 2 |> get_ship_at_coord 1 1));
  "2x2 guess"  >::
    (fun _ -> assert_equal
    false (initialize_board 2 2 |> check_guessed 1 0)); *)
  "guess immutable change"  >::
    (fun _ -> assert_equal
    true (board.(1).(0) <- {status=true; ship=None};
    board.(1).(0).status));
  "guess immutable unchanged"  >::
    (fun _ -> assert_equal
    false (board.(1).(0) <- {status=true; ship=None};
    board.(0).(0).status));
  "guess mutable"  >::
    (fun _ -> assert_equal
    true (board.(0).(1).status <- true;
    board.(0).(1).status&&board.(1).(0).status));
  "ship"  >::
    (fun _ -> assert_equal
    true (add_ship ship_a board;
      board.(0).(0).ship =
      Some Ship.({parts=[part_a]; name="small ship"})));
  "larger ship part"  >::
    (fun _ -> assert_equal
    true (add_ship ship_b board;
      board.(0).(0).ship =
      Some Ship.({parts=[part_a; part_b]; name="larger ship"})));
  "big ship all parts"  >::
    (fun _ -> assert_equal
    true (add_ship ship_c board;
      board.(0).(0) = board.(1).(0) && board.(2).(0) = board.(3).(0)
      && board.(1).(0) = board.(2).(0)));
]

let _ = run_test_tt_main tests
