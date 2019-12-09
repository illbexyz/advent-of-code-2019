open Base
open Stdio

let input_filename = "day_05/input.txt"

let main () =
  let open Result.Let_syntax in
  let%bind line =
    List.hd @@ Utils.input_lines input_filename
    |> Result.of_option ~error:"Error: Empty file"
  in

  let start_memory = String.split line ~on:',' |> List.map ~f:Int.of_string in
  let initial_state = Intcode.initial_state start_memory [ 1 ] in
  Intcode.exec initial_state |> Result.map_error ~f:Intcode.show_intcode_error

let () =
  match main () with
  | Ok final_state ->
      print_endline @@ Intcode.show_computer_state final_state;
      print_endline "Success"
  | Error err -> print_endline err
