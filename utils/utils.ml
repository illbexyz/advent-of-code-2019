let input_lines input_filename =
  let ic = open_in input_filename in
  let rec build_list l =
    match input_line ic with
    | line -> build_list (line :: l)
    | exception End_of_file -> close_in ic; List.rev l
  in
  build_list []