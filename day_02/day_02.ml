open Base
open Stdio
open Utils

let input_filename = "day_02/input.txt"

let parse_end tokens =
  if List.is_prefix tokens ~prefix:[ 99 ] ~equal:( = ) then Some Opcode.End
  else None

let parse_cmd tokens prefix =
  if List.is_prefix tokens ~prefix:[ prefix ] ~equal:( = ) then
    match tokens with _ :: x :: y :: z :: _tail -> Some (x, y, z) | _ -> None
  else None

let parse_sum tokens =
  Option.map (parse_cmd tokens 1) ~f:(fun (x, y, z) -> Opcode.Sum (x, y, z))

let parse_mult tokens =
  Option.map (parse_cmd tokens 2) ~f:(fun (x, y, z) -> Opcode.Mult (x, y, z))

let parse tokens =
  let parse_combinator tks =
    parse_sum tks <|> parse_mult tks <|> parse_end tks
  in
  parse_combinator tokens

(* Beware! This will modify the memory arg as a side effect *)
let execute_opcode memory opcode =
  let exec_cmd cmd x y z = memory.(z) <- cmd memory.(x) memory.(y) in
  match opcode with
  | Opcode.Sum (x, y, z) -> exec_cmd ( + ) x y z
  | Opcode.Mult (x, y, z) -> exec_cmd ( * ) x y z
  | Opcode.End -> ()

let exec memory noun verb =
  let memory = List.to_array memory in

  memory.(1) <- noun;
  memory.(2) <- verb;

  let seq_memory = Array.to_sequence_mutable memory in

  let rec eval mem_sequence =
    Option.(
      parse @@ Sequence.to_list mem_sequence >>= fun operation ->
      (* print_endline @@ string_of_opcode operation; *)
      execute_opcode memory operation;
      match operation with
      | End -> None
      | _ ->
          eval @@ Sequence.drop mem_sequence (Opcode.chars_consumed operation))
  in
  let _ = eval seq_memory in
  memory

let main =
  let open Result in
  let opt_line = List.hd @@ Utils.input_lines input_filename in
  Result.of_option opt_line ~error:"Error: Empty file" >>= fun line ->
  let start_memory = String.split line ~on:',' in
  let start_memory = List.map start_memory ~f:Int.of_string in

  (* Part One *)
  let p1_memory = exec start_memory 12 2 in
  print_endline @@ Int.to_string @@ p1_memory.(0);

  (* Part Two *)
  ( try
      for i = 0 to 99 do
        for j = 0 to 99 do
          let res_memory = exec start_memory i j in
          if res_memory.(0) = 19690720 then (
            print_endline @@ Int.to_string @@ ((100 * i) + j);
            raise Break )
        done
      done
    with Break -> () );

  Ok ()
