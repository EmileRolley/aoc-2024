open Core
open Day

let get_input = function
  | P1 -> "inputs/day03/inputs_part1.txt"
  | P2 -> "inputs/day03/test_part2.txt"
;;

let parse_mul lines =
  let open Angstrom in
  let mul =
    string "mul(" *> Utils.Parse.integer
    >>= fun left ->
    char ',' *> Utils.Parse.integer <* char ')' >>| fun right -> Some (left, right)
  in
  let all_muls =
    many
      (skip_while (function
         | 'm' -> false
         | _ -> true)
       *> choice [ mul; char 'm' *> return None ])
  in
  lines
  |> List.map ~f:(fun line ->
    match parse_string ~consume:Prefix all_muls line with
    | Ok str -> str
    | _ -> failwith "Could not parse line")
  |> List.join
  |> List.filter_map ~f:Fn.id
;;

let part_1 lines =
  parse_mul lines |> List.fold ~init:0 ~f:(fun acc (l, r) -> (l * r) + acc)
;;

let part_2 _ = 0

let part = function
  | P1 -> part_1
  | P2 -> part_2
;;
