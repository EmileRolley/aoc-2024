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

  let parse_lines_with parser lines =
    lines
    |> String.concat ~sep:""
    |> Angstrom.parse_string ~consume:Prefix parser
    |> Result.ok_or_failwith
  ;;
end

let get_center_elem l =
  let open Core in
  List.nth_exn l (List.length l / 2)
;;

let position_compare (x, y) (x', y') =
  match Int.compare y y' with
  | 0 -> Int.compare x x'
  | c -> c
;;

let position_equal (x, y) (x', y') = x = x' && y = y'
let position_to_string (x, y) = Printf.sprintf "(%d, %d)" x y

let string_drop_last str =
  let open Core in
  String.slice str 0 (String.length str - 1)
;;

module Matrix = struct
  open Core

  let make ~dimx ~dimy v = Array.make_matrix ~dimx ~dimy v

  let iteri m ~f =
    Array.iteri m ~f:(fun y row -> Array.iteri row ~f:(fun x elem -> f ~x ~y elem))
  ;;

  let fold_in m ~start:(i, j) ~stop:(i', j') ~init ~f =
    let res = ref init in
    let min_i = max 0 i in
    let min_j = max 0 j in
    let max_i = min (Array.length m) i' in
    let max_j = min (Array.length m.(0)) j' in
    for i'' = min_i to max_i - 1 do
      for j'' = min_j to max_j - 1 do
        res := f !res ~pos:(i'', j'') ~e:m.(i'').(j'')
      done
    done;
    !res
  ;;

  let get m (x, y) = m.(y).(x)
end

module List = struct
  let replace l ~at ~e =
    let open Core in
    let left = List.slice l 0 at in
    let right = List.slice l (at + 1) 0 in
    if at = 0 then e :: right else left @ (e :: right)
  ;;

  let remove l ~at =
    let open Core in
    if at = 0 then List.slice l 1 0 else List.slice l 0 at @ List.slice l (at + 1) 0
  ;;

  let map_n l ~f ~n =
    let rec aux l n = if n = 0 then l else aux (f l) (n - 1) in
    aux l n
  ;;
end
