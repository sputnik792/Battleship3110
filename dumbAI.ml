open Random
open Board

let () = Random.self_init ()
let guess_num row col = 
  Random.int (row*col)

let new_num = 
  let count = ref (-1) in (fun () -> incr count; !count)

let cmd_of_num n =
  let x = n/10 in
  let y = n mod 10 in
  "shoot "^(string_of_int x)^" "^(string_of_int y)

(* Dumb AI: returns a guess move as a string input for Game *)
let guess_coord (board: Board.t) (key: string) : string =
  if key = "line" then
    let two_digits = new_num () in
    cmd_of_num two_digits
  else if key = "randumb" then
    let two_digits = guess_num (Array.length board) (Array.length board.(0)) in
    cmd_of_num two_digits
  else failwith "whatwhat"