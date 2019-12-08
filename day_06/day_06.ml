open Base
open Stdio

let input_filename = "day_06/input.txt"

module Tree = struct
  type t = (string, string) Hashtbl.t

  let root = "COM"

  let empty () = Hashtbl.create (module String)

  let insert tree x parent = Hashtbl.add tree ~key:x ~data:parent

  let values tree = Hashtbl.keys tree

  let distance_between tree ancestor descendant =
    let rec inner label count =
      match String.equal label ancestor with
      | true -> count
      | false -> inner (Hashtbl.find_exn tree label) (count + 1)
    in
    inner descendant 0

  let depth_of tree label = distance_between tree root label

  let common_ancestor tree x y =
    let rec inner x y depth_of_x depth_of_y =
      match (String.equal x y, depth_of_x > depth_of_y) with
      | true, _ -> x
      | false, true ->
          inner (Hashtbl.find_exn tree x) y (depth_of_x - 1) depth_of_y
      | false, false ->
          inner x (Hashtbl.find_exn tree y) depth_of_x (depth_of_y - 1)
    in
    let depth_of_x = depth_of tree x in
    let depth_of_y = depth_of tree y in
    inner x y depth_of_x depth_of_y
end

let parse_orbit line =
  match String.split ~on:')' line with
  | [ x; y ] -> Ok (x, y)
  | _ -> Error "There should always be 2 orbits"

let main =
  let lines = Utils.input_lines input_filename in
  let orbits =
    List.map lines ~f:(fun l -> Result.ok_or_failwith (parse_orbit l))
  in
  let tree = Tree.empty () in
  List.iter orbits ~f:(fun (x, y) -> ignore @@ Tree.insert tree y x);

  (* Part One *)
  let labels = Tree.values tree in
  let sum =
    List.map labels ~f:(Tree.depth_of tree) |> List.fold ~init:0 ~f:( + )
  in
  printf "Part one: %d\n" sum;

  (* Part Two *)
  let cmn_ancestor = Tree.common_ancestor tree "SAN" "YOU" in

  let distance_from_santa =
    let distance_from_ancestor = Tree.distance_between tree cmn_ancestor in
    distance_from_ancestor "SAN" + distance_from_ancestor "YOU" - 2
  in
  printf "Part two: %d\n" distance_from_santa;
  Ok 0
