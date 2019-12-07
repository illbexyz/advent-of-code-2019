open Base

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
