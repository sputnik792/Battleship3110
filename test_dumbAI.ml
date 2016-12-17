open OUnit2
open Game
open Board
open DumbAI
open Ship

let new_board = initialize_board 10 10

new_ship name x y length 1

let carrier = Ship.new_ship "Carrier" 1 1 5 1 in
let battleship = Ship.new_ship "Battleship" 2 3 1 4 in
let cruiser = Ship.new_ship "Cruiser" 7 2 3 1 in
let submarine = Ship.new_ship "Submarine" 6 6 3 1 in
let destroyer = Ship.new_ship "Destroyer" 1 9 1 2 in
let h = Player.add_ship [carrier; battleship; cruiser; submarine; destroyer] in
List.iter (fun sh -> Board.add_ship sh new_board) h;

let tests = "test suite" >::: [
]

let _ = run_test_tt_main tests