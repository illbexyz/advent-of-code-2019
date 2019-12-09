open Base
open Stdio

let input_filename = "day_08/input.txt"

(* let explode s = List.init (String.length s) ~f:(String.get s) *)

type layer = { width : int; height : int; data : string list } [@@deriving show]

let make_layer width height data = { width; height; data }

let flatten_layer (layer : layer) =
  let { data; _ } = layer in
  data
  (* |> List.map ~f:explode *)
  |> String.concat

let length_of_layer (layer : layer) =
  let { data; _ } = layer in
  data |> List.map ~f:String.length |> List.fold ~init:0 ~f:( + )

let get_pixel (layer : layer) i j =
  let { data; _ } = layer in
  (List.nth_exn data i).[j]

type image = { width : int; height : int; layers : layer list }
[@@deriving show]

let make_image width height layers = { width; height; layers }

let count_digit_in_layer digit (layer : layer) =
  let { data; _ } = layer in
  data
  |> List.map ~f:(fun x -> String.count x ~f:(Char.equal digit))
  |> List.fold ~init:0 ~f:( + )

let render_image_to_layer (image : image) =
  let layers = List.rev image.layers in
  List.tl_exn layers
  |> List.fold ~init:(List.hd_exn layers) ~f:(fun prev_layer curr_layer ->
      let new_data =
        List.mapi curr_layer.data ~f:(fun i curr_string ->
            String.mapi curr_string ~f:(fun j curr_pixel ->
                let prev_pixel = get_pixel prev_layer i j in
                match (prev_pixel, curr_pixel) with
                | '2', y -> y
                | x, '2' -> x
                | _x, y -> y))
      in
      { curr_layer with data = new_data })

let render_layer (layer : layer) =
  layer.data
  |> List.map ~f:(String.map ~f:(fun c -> match c with '0' -> ' ' | _ -> '0'))
  |> String.concat ~sep:"\n"

let main =
  let _aaa = "€" in
  let width = 25 in
  let height = 6 in

  let lines = Utils.input_lines input_filename in
  let line = List.hd_exn lines in
  let (layers : layer list) =
    String.to_list line
    |> List.groupi ~break:(fun idx _ _ -> idx % width = 0)
    |> List.groupi ~break:(fun idx _ _ -> idx % height = 0)
    |> List.map ~f:(List.map ~f:String.of_char_list)
    |> List.map ~f:(make_layer width height)
  in
  let (image : image) = make_image width height layers in
  (* print_endline @@ show_image image; *)
  let fewest_zeros_layer, _ =
    image.layers
    |> List.map ~f:(fun layer -> (layer, count_digit_in_layer '0' layer))
    |> List.min_elt ~compare:(fun (_, res1) (_, res2) -> Int.compare res1 res2)
    |> Option.value_exn
  in
  (* printf "%s\n" (show_layer @@ fewest_zeros_layer); *)
  printf "Part one: %d\n"
    ( count_digit_in_layer '1' fewest_zeros_layer
      * count_digit_in_layer '2' fewest_zeros_layer );

  print_endline "Part two:\n";
  print_endline @@ render_layer (render_image_to_layer image);
  print_endline "";
