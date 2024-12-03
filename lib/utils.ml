(** Read input from a file *)
let read_input input =
  let file = open_in input in
  let rec read_lines acc =
    try
      let line = input_line file in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  close_in file;
  lines
;;

let parse fmt map line =
  try Some (Scanf.sscanf line fmt map) with
  | _ -> None
;;

(** Try to parse a line with a list of parsers. If none of the parsers can parse the line, an exception is raised.

    {[
      let parser = try_parse [ parse "%d" (fun x -> `Num x) ] "42"
    ]} *)
let rec try_parse parsers line =
  match parsers with
  | [] -> failwith ("Could not parse line: " ^ line)
  | parser :: parsers ->
    (match parser line with
     | None -> try_parse parsers line
     | Some result -> result)
;;
