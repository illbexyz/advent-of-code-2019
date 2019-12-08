open Base
open Computer

let parse_cmd state suffix n_args =
  let open Result.Let_syntax in
  let token = Int.to_string @@ state.memory.(state.ip) in
  let code = Int.of_string @@ String.suffix token 2 in
  match code = suffix && String.length token <= 5 with
  | true ->
      let modes = String.to_list @@ String.drop_suffix token 2 in
      let%bind n_missing_zeros =
        match n_args - List.length modes >= 0 with
        | true -> Ok (n_args - List.length modes)
        | false -> Error ""
      in
      let complement = List.init n_missing_zeros ~f:(fun _ -> '0') in
      let modes = List.append complement modes in
      let params =
        List.init n_args ~f:(fun i -> state.memory.(state.ip + i + 1))
      in
      List.zip_exn (List.rev modes) params
      |> List.map ~f:(function
           | '0', x -> Ok (Position x)
           | '1', y -> Ok (Immediate y)
           | _ ->
               Error
                 "Parser error: The paramer type (position or immediate) can \
                  be only 0 or 1")
      |> Result.all
  | false ->
      Error
        (Printf.sprintf
           "Parser error: Could not parse the next input: %s\nMemory:\n%s\n"
           token
           (show_computer_state state))

let parse_end state = parse_cmd state 99 0 |> Result.map ~f:(fun _ -> End)

let parse_op_3 state suffix =
  let open Result.Let_syntax in
  let%bind args = parse_cmd state suffix 3 in
  match args with
  | [ x; y; z ] -> Ok (x, y, z)
  | _ ->
      Error "Something weird happened, there should be 3 args here (parse_op_3)"

let parse_op_2 state suffix =
  let open Result.Let_syntax in
  let%bind args = parse_cmd state suffix 2 in
  match args with
  | [ x; y ] -> Ok (x, y)
  | _ ->
      Error "Something weird happened, there should be 2 args here (parse_op_2)"

let parse_op_1 state suffix =
  let open Result.Let_syntax in
  let%bind args = parse_cmd state suffix 1 in
  match args with
  | [ x ] -> Ok x
  | _ ->
      Error "Something weird happened, there should be 1 args here (parse_op_1)"

let parse_sum state =
  Result.map (parse_op_3 state 1) ~f:(fun (x, y, z) -> Sum (x, y, z))

let parse_mult state =
  Result.map (parse_op_3 state 2) ~f:(fun (x, y, z) -> Mult (x, y, z))

let parse_input state = Result.map (parse_op_1 state 3) ~f:(fun x -> Input x)

let parse_output state = Result.map (parse_op_1 state 4) ~f:(fun x -> Output x)

let parse_jump_if_true state =
  Result.map (parse_op_2 state 5) ~f:(fun (x, y) -> JumpIfTrue (x, y))

let parse_jump_if_false state =
  Result.map (parse_op_2 state 6) ~f:(fun (x, y) -> JumpIfFalse (x, y))

let parse_less_than state =
  Result.map (parse_op_3 state 7) ~f:(fun (x, y, z) -> LessThan (x, y, z))

let parse_equal state =
  Result.map (parse_op_3 state 8) ~f:(fun (x, y, z) -> Equals (x, y, z))

let parsers =
  [
    parse_sum;
    parse_mult;
    parse_end;
    parse_input;
    parse_output;
    parse_jump_if_true;
    parse_jump_if_false;
    parse_less_than;
    parse_equal;
  ]

let parser_of = function
  | Sum _ -> parse_sum
  | Mult _ -> parse_mult
  | Input _ -> parse_input
  | Output _ -> parse_output
  | JumpIfTrue _ -> parse_jump_if_true
  | JumpIfFalse _ -> parse_jump_if_false
  | LessThan _ -> parse_less_than
  | Equals _ -> parse_equal
  | End -> parse_end

let parse state =
  List.fold_until parsers ~init:(Error "")
    ~finish:(fun acc -> acc)
    ~f:(fun last_result curr_parser ->
      match last_result with
      | Error _ -> Continue (curr_parser state)
      | x -> Stop x)
  |> Result.map_error ~f:(fun err -> ParserError err)
