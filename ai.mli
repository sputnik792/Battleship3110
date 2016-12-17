open Ship
open Board


type ai_record = {prev_coord_guess: int*int; counter_tup: int*bool;
  ship_list: Ship.t list;	min_number: int; max_number: int;
  game_grid: tile array array ; attack_flag: bool;
	hit_targets: ((int*int)*bool) list; main_axis: (int*int) list;
  axis_dir: int; firstflag: bool }


(*[game_turn rec] returns all the information related to the AI's current attack
target in the form of an ai_record*)
val game_turn : ai_record -> ai_record



