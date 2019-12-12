open Base

type param = Immediate of int | Position of int | Relative of int
[@@deriving show]

let int_of_param = function
  | Immediate x -> x
  | Position x -> x
  | Relative x -> x

type cmd =
  | Sum of param * param * param
  | Mult of param * param * param
  | Input of param
  | Output of param
  | JumpIfTrue of param * param
  | JumpIfFalse of param * param
  | LessThan of param * param * param
  | Equals of param * param * param
  | ChangeRB of param
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
  | ChangeRB _ -> 2
  | End -> 1

type computer_state = {
  memory : int array;
  std_in : int list;
  std_out : int list;
  ip : int;
  rb : int;
}
[@@deriving sexp_of]

let show_computer_state state =
  Sexp.to_string_hum (sexp_of_computer_state state)

type intcode_error = NoInput of computer_state | ParserError of string

let show_intcode_error = function
  | NoInput state -> show_computer_state state
  | ParserError err -> err
