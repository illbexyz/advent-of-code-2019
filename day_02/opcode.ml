open Base

type t = Sum of int * int * int | Mult of int * int * int | End

let chars_consumed = function Sum _ -> 4 | Mult _ -> 4 | End -> 1

let to_string = function
  | Sum (x, y, z) ->
      String.concat ~sep:" "
        [ "Sum"; Int.to_string x; Int.to_string y; Int.to_string z ]
  | Mult (x, y, z) ->
      String.concat ~sep:" "
        [ "Mult"; Int.to_string x; Int.to_string y; Int.to_string z ]
  | End -> "End"
