open Base
open Stdio

let input_filename = "day_03/input.txt"

module Int_tuple = struct
  type t = int * int [@@deriving compare, sexp_of, hash]
end

type path = Right of int | Up of int | Left of int | Down of int

let value_of_path path =
  match path with Right x -> x | Up x -> x | Left x -> x | Down x -> x

let parse_path token =
  let mask =
    List.map
      ~f:(fun prefix -> String.is_prefix token ~prefix)
      [ "R"; "U"; "L"; "D" ]
  in
  let parse_length = Int.of_string @@ String.drop_prefix token 1 in
  let opt_path =
    match mask with
    | true :: _tl -> Some (Right parse_length)
    | false :: true :: _tl -> Some (Up parse_length)
    | false :: false :: true :: _tl -> Some (Left parse_length)
    | false :: false :: false :: true :: _tl -> Some (Down parse_length)
    | _ -> None
  in
  Option.value_exn opt_path

let manhattan_distance pos1 pos2 =
  let x1, y1 = pos1 in
  let x2, y2 = pos2 in
  abs (x1 - x2) + abs (y1 - y2)

let update_table table curr_pos path line_idx steps =
  let curr_x, curr_y = curr_pos in
  let update_key op is_horizontal i =
    if is_horizontal then (op curr_x i, curr_y) else (curr_x, op curr_y i)
  in
  let value = value_of_path path in
  let final_pos = ref curr_pos in
  for i = 1 to value do
    let key =
      match path with
      | Right _ -> update_key ( + ) true i
      | Left _ -> update_key ( - ) true i
      | Up _ -> update_key ( + ) false i
      | Down _ -> update_key ( - ) false i
    in
    let prev_line_idx, prev_value, prev_total_steps =
      Hashtbl.find_or_add table key ~default:(fun () ->
          (line_idx, 0, steps + i))
    in
    final_pos := key;
    if prev_line_idx <> line_idx then
      Hashtbl.set table ~key
        ~data:(line_idx, prev_value + 1, steps + i + prev_total_steps)
  done;
  (!final_pos, steps + value)

let main =
  let open Result in
  let lines = Utils.input_lines input_filename in
  let table = Hashtbl.create (module Int_tuple) in
  let start_pos = (0, 0) in
  let rec update_table_loop tokens pos line_idx steps =
    match tokens with
    | token :: tl ->
        let path = parse_path token in
        let updated_pos, updated_steps =
          update_table table pos path line_idx steps
        in
        update_table_loop tl updated_pos line_idx updated_steps
    | [] -> ()
  in

  List.iteri
    ~f:(fun idx line ->
      let tokens = String.split line ~on:',' in
      update_table_loop tokens start_pos idx 0)
    lines;

  let crosses_with_dist =
    Hashtbl.to_alist table
    |> List.filter_map ~f:(fun ((x, y), (_, value, tot_steps)) ->
           if value > 0 then
             Some (manhattan_distance start_pos (x, y), (x, y), tot_steps)
           else None)
  in

  (* Hashtbl.iteri table ~f:(fun ~key ~data ->
      let _, data, _ = data in
      let x, y = key in
      if data > 0 then
        printf "Pos: (%d %d), Distance: %d\n" x y
          (manhattan_distance start_pos (x, y))); *)

  (* Part One *)
  let min_distance, (key_x, key_y), _ =
    List.min_elt crosses_with_dist ~compare:(fun (x, _, _) (y, _, _) ->
        Int.compare x y)
    |> Option.value_exn
  in
  printf "Shortest distance: %d, Pos: (%d %d)\n" min_distance key_x key_y;

  (* Part Two *)
  let _, (key_x, key_y), num_steps =
    List.min_elt crosses_with_dist ~compare:(fun (_, _, x) (_, _, y) ->
        Int.compare x y)
    |> Option.value_exn
  in
  printf "Shortest steps: %d, Pos: (%d %d)\n" num_steps key_x key_y;

  Ok ()
