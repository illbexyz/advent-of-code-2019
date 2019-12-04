open Base
open Stdio

let rec get_digits ?(acc = []) num =
  let div = num / 10 in
  let digit = Int.rem num 10 in
  if div > 0 then get_digits ~acc:(digit :: acc) div else digit :: acc

let is_password_p1 num =
  let digits = get_digits num in
  let pairs =
    List.map2_exn
      (List.take digits (List.length digits - 1))
      (List.tl_exn digits)
      ~f:(fun x1 x2 -> (x1, x2))
  in
  let is_length_ok = List.length digits = 6 in
  let is_nondecreasing_ok = List.for_all pairs ~f:(fun (x, y) -> x <= y) in
  let is_adjacent_ok = List.exists pairs ~f:(fun (x, y) -> x = y) in
  is_length_ok && is_nondecreasing_ok && is_adjacent_ok

let is_password_p2 num =
  let digits = get_digits num in
  digits
  |> List.dedup_and_sort ~compare:compare_int
  |> List.map ~f:(fun x -> List.count digits ~f:(fun y -> x = y))
  |> List.exists ~f:(fun x -> x = 2)

let main =
  let start_num = 372304 in
  let end_num = 847060 in
  let passwords =
    List.range start_num end_num ~start:`inclusive ~stop:`inclusive
    |> List.filter ~f:is_password_p1
  in
  (* Part One *)
  printf "Number of passwords (p1): %d\n" @@ List.length passwords;
  (* Part Two *)
  let passwords_p2 = passwords |> List.filter ~f:is_password_p2 in
  printf "Number of passwords (p2): %d\n" @@ List.length passwords_p2
