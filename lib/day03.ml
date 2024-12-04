open Core
open Day

let get_input = function
  | P1 -> "inputs/day03/inputs_part1.txt"
  | P2 -> "inputs/day03/inputs_part2.txt"
;;

let parse_mul =
  let open Angstrom in
  string "mul(" *> Utils.Parse.integer
  >>= fun left ->
  char ',' *> Utils.Parse.integer <* char ')' >>| fun right -> Some (left, right)
;;

let parse_lines_with parser lines =
  lines
  |> String.concat ~sep:""
  |> Angstrom.parse_string ~consume:Prefix parser
  |> Result.ok_or_failwith
;;

let parse_all_mul =
  parse_lines_with Angstrom.(many (parse_mul <|> any_char *> return None))
;;

let parse_all_enabled_mul =
  let open Angstrom in
  let skip_dont = string "don't()" *> many_till any_char (string "do()") in
  parse_lines_with
    (many (skip_dont *> return None <|> parse_mul <|> any_char *> return None))
;;

let part p lines =
  (match p with
   | P1 -> parse_all_mul lines
   | P2 -> parse_all_enabled_mul lines)
  |> List.filter_map ~f:Fn.id
  |> List.fold ~init:0 ~f:(fun acc (l, r) -> (l * r) + acc)
;;
