open Base

let input_filename = "day_05/input.txt"

let main =
  let open Result.Let_syntax in
  let%bind line =
    List.hd @@ Utils.input_lines input_filename
    |> Result.of_option ~error:"Error: Empty file" in

  let start_memory =
    String.split line ~on:',' 
    |> List.map ~f:Int.of_string in

  let _ = Intcode.exec start_memory in

  Ok ()
