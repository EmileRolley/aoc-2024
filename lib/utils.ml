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

let scan fmt map line =
  try Some (Scanf.sscanf line fmt map) with
  | _ -> None
;;

(** Try to parse a line with a list of parsers. If none of the parsers can parse the line, an exception is raised.

    {[
      let parser = try_parse [ parse "%d" (fun x -> `Num x) ] "42"
    ]} *)
let rec try_scan scanners line =
  match scanners with
  | [] -> failwith ("Could not parse line: " ^ line)
  | scan :: scanners ->
    (match scan line with
     | None -> try_scan scanners line
     | Some result -> result)
;;

(** Return a new array without the element at index [i].

    {[
      let arr = [| 1; 2; 3 |] in
      let arr' = array_remove arr 1 in
      assert (arr' = [| 1; 3 |])
    ]} *)
let array_remove arr i =
  if i = 0
  then Core.Array.slice arr 1 0
  else Core.Array.concat [ Core.Array.slice arr 0 i; Core.Array.slice arr (i + 1) 0 ]
;;

module Parse = struct
  open Core
  open Angstrom

  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| Int.of_string
  ;;
end

let consume_if ~cond ~f x = if cond then f x else ()

let cond_apply_and_sum_all fs =
  let open Core in
  List.fold ~init:0 ~f:(fun acc (cond, f) -> if cond then acc + f () else acc) fs
;;
