let input_file = "./day_one/input.txt"

let fuel (mass : int) : int =
  let dividedByThree = float_of_int mass /. 3.0 in
  let roundedDown = int_of_float @@ floor dividedByThree in
  roundedDown - 2

let rec fuel_of_fuel mass =
  let rem = fuel mass in
  if rem > 0 then fuel_of_fuel rem + rem else 0

let lines = Utils.input_lines input_file

let () =
  let sum =
    List.fold_right (fun x y -> (fuel @@ int_of_string x) + y) lines 0
  in
  print_endline @@ string_of_int sum

let () =
  let sum =
    List.fold_right (fun x y -> (fuel_of_fuel @@ int_of_string x) + y) lines 0
  in
  print_endline @@ string_of_int sum
