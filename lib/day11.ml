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

let blink = function
  | 0 -> [ 1 ]
  | n ->
    (match split_stone n with
     | Some (a, b) -> [ Int.of_string a; Int.of_string b ]
     | None -> [ n * 2024 ])
;;

let memoized_blink_n stones ~n =
  let memo = Hashtbl.create (module String) in
  let rec aux s ~n =
    let key = Printf.sprintf "%d-%d" n s in
    match Hashtbl.find memo key with
    | Some v -> v
    | None ->
      let res = if n = 0 then 1 else blink s |> Utils.List.sum ~f:(aux ~n:(n - 1)) in
      Hashtbl.set memo ~key ~data:res;
      res
  in
  Utils.List.sum stones ~f:(aux ~n)
;;

let part part lines =
  let n =
    match part with
    | P1 -> 25
    | P2 -> 75
  in
  parse_stones (List.hd_exn lines) |> memoized_blink_n ~n
;;
