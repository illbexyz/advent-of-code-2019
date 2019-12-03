open Base
open Stdio

let input_lines input_filename =
  let ic = In_channel.create input_filename in
  In_channel.input_lines ic

let rec print_list ~to_string = function
  | x :: tail ->
      print_string @@ to_string x;
      print_string " ";
      print_list tail ~to_string
  | [] -> print_string "\n"

(* Option's alternative operator *)
let ( <|> ) x y =
  match (x, y) with
  | None, None -> None
  | Some xx, None -> Some xx
  | None, Some yy -> Some yy
  | Some xx, Some _yy -> Some xx

exception Break
