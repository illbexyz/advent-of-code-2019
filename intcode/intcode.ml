open Base
open Utils
open Stdio

type param = Immediate of int | Position of int [@@deriving show]

type t =
  | Sum of param * param * param
  | Mult of param * param * param
  | Input of param
  | Output of param
  | JumpIfTrue of param * param
  | JumpIfFalse of param * param
  | LessThan of param * param * param
  | Equals of param * param * param
  | End
[@@deriving show]

let chars_consumed = function
  | Sum _ -> 4
  | Mult _ -> 4
  | Input _ -> 2
  | Output _ -> 2
  | JumpIfTrue _ -> 3
  | JumpIfFalse _ -> 3
  | LessThan _ -> 4
  | Equals _ -> 4
  | End -> 1

let int_of_param = function Immediate x -> x | Position x -> x

let parse_cmd memory ip suffix n_args =
  let token = Int.to_string @@ memory.(ip) in
  let code = Int.of_string @@ String.suffix token 2 in
  match code = suffix && String.length token <= 5 with
  | true ->
    let modes = String.to_list @@ String.drop_suffix token 2 in
    let complement =
      List.init (n_args - List.length modes) ~f:(fun _ -> '0')
    in
    let modes = List.append complement modes in
    let params = List.init n_args ~f:(fun i -> memory.(ip + i + 1)) in
    List.zip_exn (List.rev modes) params
    |> List.map ~f:(function
        | '0', x -> Position x
        | '1', y -> Immediate y
        | _ -> failwith "should not reach this")
    |> Some
  | false -> None

let parse_end memory ip =
  parse_cmd memory ip 99 0 |> Option.map ~f:(fun _ -> End)

let parse_op_3 memory ip suffix =
  let open Option.Let_syntax in
  let%bind args = parse_cmd memory ip suffix 3 in
  match args with [ x; y; z ] -> Some (x, y, z) | _ -> None

let parse_op_2 memory ip suffix =
  let open Option.Let_syntax in
  let%bind args = parse_cmd memory ip suffix 2 in
  match args with [ x; y ] -> Some (x, y) | _ -> None

let parse_op_1 memory ip suffix =
  let open Option.Let_syntax in
  let%bind args = parse_cmd memory ip suffix 1 in
  match args with [ x ] -> Some x | _ -> None

let parse_sum memory ip =
  Option.map (parse_op_3 memory ip 1) ~f:(fun (x, y, z) -> Sum (x, y, z))

let parse_mult memory ip =
  Option.map (parse_op_3 memory ip 2) ~f:(fun (x, y, z) -> Mult (x, y, z))

let parse_input memory ip =
  Option.map (parse_op_1 memory ip 3) ~f:(fun x -> Input x)

let parse_output memory ip =
  Option.map (parse_op_1 memory ip 4) ~f:(fun x -> Output x)

let parse_jump_if_true memory ip =
  Option.map (parse_op_2 memory ip 5) ~f:(fun (x, y) -> JumpIfTrue (x, y))

let parse_jump_if_false memory ip =
  Option.map (parse_op_2 memory ip 6) ~f:(fun (x, y) -> JumpIfFalse (x, y))

let parse_less_than memory ip =
  Option.map (parse_op_3 memory ip 7) ~f:(fun (x, y, z) -> LessThan (x, y, z))

let parse_equal memory ip =
  Option.map (parse_op_3 memory ip 8) ~f:(fun (x, y, z) -> Equals (x, y, z))

let parse memory ip =
  parse_sum memory ip <|> parse_mult memory ip <|> parse_end memory ip
  <|> parse_input memory ip <|> parse_output memory ip
  <|> parse_jump_if_true memory ip
  <|> parse_jump_if_false memory ip
  <|> parse_less_than memory ip <|> parse_equal memory ip

let get_arg memory p =
  match p with Immediate x -> x | Position x -> memory.(x)

let exec_bin_op memory cmd x y z =
  let p1 = get_arg memory x in
  let p2 = get_arg memory y in
  let p3 = int_of_param z in
  memory.(p3) <- cmd p1 p2

let exec_sum memory x y z = exec_bin_op memory ( + ) x y z

let exec_mult memory x y z = exec_bin_op memory ( * ) x y z

let exec_input memory param =
  let arg = int_of_param param in
  print_string "Input: ";
  let inp = Caml.read_int () in
  (* let inp = 1 in *)
  memory.(arg) <- inp

let exec_output memory param =
  let p1 = int_of_param param in
  printf "Output: %d\n" memory.(p1)

let exec_jump memory x y op =
  let p1 = get_arg memory x in
  let p2 = get_arg memory y in
  match op p1 with true -> Some p2 | false -> None

let exec_jump_if_true memory x y = exec_jump memory x y (( <> ) 0)

let exec_jump_if_false memory x y = exec_jump memory x y (( = ) 0)

let exec_bin_bool_op memory x y z cond =
  let p1 = get_arg memory x in
  let p2 = get_arg memory y in
  let p3 = int_of_param z in
  let bit = match cond p1 p2 with true -> 1 | false -> 0 in
  memory.(p3) <- bit

let exec_less_than memory x y z = exec_bin_bool_op memory x y z ( < )

let exec_equals memory x y z = exec_bin_bool_op memory x y z ( = )

(* Beware! This will modify the memory arg as a side effect *)
let execute_opcode memory ip opcode =
  let opt_new_ip =
    match opcode with
    | Sum (x, y, z) ->
      exec_sum memory x y z;
      None
    | Mult (x, y, z) ->
      exec_mult memory x y z;
      None
    | Input x ->
      exec_input memory x;
      None
    | Output x ->
      exec_output memory x;
      None
    | JumpIfTrue (x, y) ->
      let open Option.Let_syntax in
      let%map ip = exec_jump_if_true memory x y in
      ip
    | JumpIfFalse (x, y) ->
      let open Option.Let_syntax in
      let%map ip = exec_jump_if_false memory x y in
      ip
    | LessThan (x, y, z) ->
      exec_less_than memory x y z;
      None
    | Equals (x, y, z) ->
      exec_equals memory x y z;
      None
    | End -> None
  in
  Option.value opt_new_ip ~default:ip

let exec (memory : int list) =
  let memory = List.to_array memory in

  let rec eval : int -> unit =
    fun ip ->
      Option.value_exn
        ~message:"Parsing failure"
        Option.(
          parse memory ip >>= fun operation ->
          (* print_endline @@ Intcode.show operation; *)
          let new_ip = execute_opcode memory ip operation in
          match operation with
          | End -> Some ()
          | JumpIfFalse _
          | JumpIfTrue _ when new_ip <> ip -> Some (eval new_ip)
          | _ -> Some (eval (new_ip + chars_consumed operation)))
  in

  let ip = 0 in
  let _ = eval ip in
  memory