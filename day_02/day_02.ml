open Base
open Stdio
open Utils

let input_filename = "day_02/input.txt"

let exec memory noun verb =
  let hd = List.hd_exn memory in
  let no_first_three = List.drop memory 3 in
  let init_state =
    Intcode.initial_state (hd :: noun :: verb :: no_first_three) []
  in
  Intcode.exec init_state

let main =
  let open Result in
  let open Result.Let_syntax in
  let%bind line =
    List.hd @@ Utils.input_lines input_filename
    |> of_option ~error:"Error: Empty file"
  in

  let start_memory = String.split line ~on:',' |> List.map ~f:Int.of_string in

  (* Part One *)
  let%bind final_state =
    map_error ~f:Intcode.show_intcode_error @@ exec start_memory 12 2
  in
  print_endline @@ Int.to_string @@ final_state.memory.(0);

  (* Part Two *)
  ( try
      for i = 0 to 99 do
        for j = 0 to 99 do
          let res_state =
            exec start_memory i j
            |> map_error ~f:Intcode.show_intcode_error
            |> ok_or_failwith
          in
          if res_state.memory.(0) = 19690720 then (
            print_endline @@ Int.to_string @@ ((100 * i) + j);
            raise Break )
        done
      done
    with Break -> () );

  Ok ()
