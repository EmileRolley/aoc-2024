open Core
open Day

let get_input = function
  | _ -> "inputs/day11/input.txt"
;;

let parse_stones line = String.split line ~on:' ' |> List.map ~f:Int.of_string

let split_stone stone =
  let s = Int.to_string stone in
  let s_len = String.length s in
  if s_len mod 2 = 0
  then Some (String.slice s 0 (s_len / 2), String.slice s (s_len / 2) s_len)
  else None
;;

let blink stones =
  List.map stones ~f:(function
    | 0 -> [ 1 ]
    | n ->
      (match split_stone n with
       | Some (a, b) -> [ Int.of_string a; Int.of_string b ]
       | None -> [ n * 2024 ]))
  |> List.join
;;

let part part lines =
  let n =
    match part with
    | P1 -> 25
    | P2 -> 75
  in
  parse_stones (List.hd_exn lines)
  |> List.map ~f:(fun l -> Utils.List.map_n [ l ] ~f:blink ~n)
  |> List.join
  |> List.length
;;
