open Base
open Stdio

let input_filename = "day_07/input.txt"

let get_digits num =
  let rec inner num acc =
    let div = num / 10 in
    let digit = Int.rem num 10 in
    match div > 0 with
    | true -> inner div (digit :: acc)
    | false -> digit :: acc
  in
  inner num []

let int_of_digits digits =
  List.foldi (List.rev digits) ~init:0 ~f:(fun idx acc x ->
      acc + (x * (10 ** idx)))

let prepend_zeros_until how_many nums =
  let zeros = List.init (how_many - List.length nums) ~f:(fun _ -> 0) in
  List.append zeros nums

type intlist = int list [@@deriving show]

(* interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ] *)
let rec interleave x lst =
  match lst with
  | [] -> [ [ x ] ]
  | hd :: tl -> (x :: lst) :: List.map ~f:(fun y -> hd :: y) (interleave x tl)

let rec permutations lst =
  match lst with
  | hd :: tl -> List.concat (List.map ~f:(interleave hd) (permutations tl))
  | _ -> [ lst ]

let exec start_memory digits =
  print_endline @@ show_intlist digits;
  let open Result.Let_syntax in
  List.fold digits ~init:(Ok 0) ~f:(fun res_signal phase ->
      let%bind signal = res_signal in
      let%bind final_state =
        Result.map_error ~f:Intcode.show_intcode_error
        @@ Intcode.exec (Intcode.initial_state start_memory [ phase; signal ])
      in
      let%map output =
        Result.of_option ~error:"No output found" @@ List.hd final_state.std_out
      in
      output)

let exec_feedback_loop start_memory digits =
  print_endline @@ show_intlist digits;
  let amps =
    List.map digits ~f:(fun digit ->
        Intcode.initial_state start_memory [ digit ])
    |> Array.of_list
  in
  let rec loop input iteration =
    let curr_idx = iteration % Array.length amps in
    let curr_amp = Intcode.add_input_to_state input amps.(curr_idx) in
    (* let _ = Caml.read_line () in *)
    print_endline "Curr amp:";
    print_endline @@ Intcode.show_computer_state curr_amp;
    print_endline "";
    let final_state_or_failed =
      Result.try_with (fun () -> Intcode.exec curr_amp)
    in
    match final_state_or_failed with
    | Ok (Error (NoInput final_state))
    (* The intcode computer is waiting for an input *)
    | Ok (Ok final_state) -> (
        match Intcode.read_output final_state with
        | Some (output, final_state) ->
            amps.(curr_idx) <- final_state;
            loop output (iteration + 1)
        | None -> Ok input )
    | Ok (Error _)
    (* The intcode computer has thrown an exception: we have found the last number *)
    | Error _ ->
        Ok input
  in
  loop 0 0

let main () =
  let open Result.Let_syntax in
  let%bind line =
    List.hd @@ Utils.input_lines input_filename
    |> Result.of_option ~error:"Error: Empty file"
  in

  let start_memory = String.split line ~on:',' |> List.map ~f:Int.of_string in

  let%bind results =
    permutations [ 0; 1; 2; 3; 4 ]
    |> List.map ~f:(prepend_zeros_until 5)
    |> List.map ~f:(exec start_memory)
    |> Result.all
  in

  let%bind p1_result =
    results
    |> List.max_elt ~compare:Int.compare
    |> Result.of_option ~error:"Something went wrong in finding the max"
  in

  let%bind results =
    permutations [ 5; 6; 7; 8; 9 ]
    |> List.map ~f:(prepend_zeros_until 5)
    |> List.map ~f:(exec_feedback_loop start_memory)
    (* |> List.map ~f:(Result.map_error ~f:Intcode.show_intcode_error) *)
    |> Result.all
  in

  let%map p2_result =
    results
    |> List.max_elt ~compare:Int.compare
    |> Result.of_option ~error:"Something went wrong in finding the max"
  in

  (p1_result, p2_result)

(* exec_feedback_loop start_memory [9;8;7;6;5] *)

let () =
  match main () with
  | Ok (p1_res, p2_res) -> printf "Part One: %d\nPart Two: %d\n" p1_res p2_res
  | Error err ->
      print_endline "Error: ";
      print_endline err
