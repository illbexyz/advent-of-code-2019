open Base

(* open Stdio *)
open Computer

let show_computer_state = Computer.show_computer_state

let show_intcode_error = Computer.show_intcode_error

let read_input state =
  match List.hd state.std_in with
  | None -> Error (NoInput state)
  | Some input -> Ok (input, { state with std_in = List.tl_exn state.std_in })

let get_arg memory p =
  match p with Immediate x -> x | Position x -> memory.(x)

let exec_bin_op state cmd x y z =
  let p1 = get_arg state.memory x in
  let p2 = get_arg state.memory y in
  let p3 = int_of_param z in
  state.memory.(p3) <- cmd p1 p2;
  Ok state

let exec_sum state x y z = exec_bin_op state ( + ) x y z

let exec_mult state x y z = exec_bin_op state ( * ) x y z

let exec_input state param =
  let open Result.Let_syntax in
  let arg = int_of_param param in
  let%map inp, state' = read_input state in
  (* printf "Input: %d\n" inp; *)
  state'.memory.(arg) <- inp;
  state'

let exec_output state param =
  let p1 = int_of_param param in
  let out = state.memory.(p1) in
  (* printf "Output: %d\n" out; *)
  Ok { state with std_out = out :: state.std_out }

let exec_jump state x y op tokens_consumed =
  let p1 = get_arg state.memory x in
  let p2 = get_arg state.memory y in
  Ok
    ( match op p1 with
    | true -> { state with ip = p2 }
    | false -> { state with ip = state.ip + tokens_consumed } )

let exec_jump_if_true state x y =
  exec_jump state x y (( <> ) 0) (chars_consumed (JumpIfFalse (x, y)))

let exec_jump_if_false state x y =
  exec_jump state x y (( = ) 0) (chars_consumed (JumpIfTrue (x, y)))

let exec_bin_bool_op state x y z cond =
  let p1 = get_arg state.memory x in
  let p2 = get_arg state.memory y in
  let p3 = int_of_param z in
  let bit = match cond p1 p2 with true -> 1 | false -> 0 in
  state.memory.(p3) <- bit;
  Ok state

let exec_less_than state x y z = exec_bin_bool_op state x y z ( < )

let exec_equals state x y z = exec_bin_bool_op state x y z ( = )

let execute_opcode state opcode =
  let open Result.Let_syntax in
  (* print_endline @@ show_cmd opcode; *)
  let%bind next_state =
    match opcode with
    | Sum (x, y, z) -> exec_sum state x y z
    | Mult (x, y, z) -> exec_mult state x y z
    | Input x -> exec_input state x
    | Output x -> exec_output state x
    | JumpIfTrue (x, y) -> exec_jump_if_true state x y
    | JumpIfFalse (x, y) -> exec_jump_if_false state x y
    | LessThan (x, y, z) -> exec_less_than state x y z
    | Equals (x, y, z) -> exec_equals state x y z
    | End -> Ok state
  in
  let next_ip =
    match opcode with
    | JumpIfTrue _ | JumpIfFalse _ -> next_state.ip
    | _ -> next_state.ip + chars_consumed opcode
  in
  Ok { next_state with ip = next_ip }

let initial_state memory std_in =
  { memory = List.to_array memory; std_in; std_out = []; ip = 0 }

let add_input_to_state input state =
  { state with std_in = List.rev (input :: state.std_in) }

let read_output state =
  match state.std_out with
  | [] -> None
  | hd :: tl -> Some (hd, { state with std_out = tl })

let exec init_state =
  let open Result.Let_syntax in
  let rec eval state =
    let%bind cmd = Parser.parse state in
    let result = execute_opcode state cmd in
    match result with
    | Ok new_state -> (
        match cmd with End -> Ok new_state | _ -> eval new_state )
    | Error err -> Error err
    (* print_endline @@ show_computer_state new_state; *)
  in

  eval init_state

(* Uncomment to reverse the output *)
(* let%map final_state = eval initial_state in
   { final_state with std_out = List.rev final_state.std_out } *)

let test_intcode memory input expected_output =
  let rslt_final_state = exec (initial_state memory input) in
  match rslt_final_state with
  | Ok final_state ->
      let actual = List.hd_exn final_state.std_out in
      expected_output = actual
  | Error _ -> false

let%test "equal position 1" =
  test_intcode [ 3; 9; 8; 9; 10; 9; 4; 9; 99; -1; 8 ] [ 8 ] 1

let%test "equal position 2" =
  test_intcode [ 3; 9; 8; 9; 10; 9; 4; 9; 99; -1; 8 ] [ 4 ] 0

let%test "less_than position 1" =
  test_intcode [ 3; 9; 7; 9; 10; 9; 4; 9; 99; -1; 8 ] [ 6 ] 1

let%test "less_than position 1" =
  test_intcode [ 3; 9; 7; 9; 10; 9; 4; 9; 99; -1; 8 ] [ 8 ] 0

let%test "equal immediate 1" =
  test_intcode [ 3; 3; 1108; -1; 8; 3; 4; 3; 99 ] [ 8 ] 1

let%test "equal_immediate 2" =
  test_intcode [ 3; 3; 1108; -1; 8; 3; 4; 3; 99 ] [ 7 ] 0

let%test "less_than immediate 2" =
  test_intcode [ 3; 3; 1107; -1; 8; 3; 4; 3; 99 ] [ 7 ] 1

let%test "less_than immediate 2" =
  test_intcode [ 3; 3; 1107; -1; 8; 3; 4; 3; 99 ] [ 8 ] 0

let%test "jump position 1" =
  test_intcode
    [ 3; 12; 6; 12; 15; 1; 13; 14; 13; 4; 13; 99; -1; 0; 1; 9 ]
    [ 0 ] 0

let%test "jump position 2" =
  test_intcode
    [ 3; 12; 6; 12; 15; 1; 13; 14; 13; 4; 13; 99; -1; 0; 1; 9 ]
    [ 1 ] 1

let%test "jump immediate 1" =
  test_intcode [ 3; 3; 1105; -1; 9; 1101; 0; 0; 12; 4; 12; 99; 1 ] [ 0 ] 0

let%test "jump immediate 2" =
  test_intcode [ 3; 3; 1105; -1; 9; 1101; 0; 0; 12; 4; 12; 99; 1 ] [ 1 ] 1
