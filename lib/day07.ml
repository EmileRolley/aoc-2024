open Day
open Core

let get_input = function
  | _ -> "inputs/day07/input.txt"
;;

let parse_equation lines =
  List.map lines ~f:(fun line ->
    let parts = String.split ~on:':' line in
    let result = Int.of_string (List.nth_exn parts 0) in
    let nums =
      List.nth_exn parts 1
      |> String.split ~on:' '
      |> List.tl_exn
      |> List.map ~f:Int.of_string
    in
    result, nums)
;;

let part part lines =
  let ( || ) n n' = Int.of_string (Int.to_string n ^ Int.to_string n') in
  let operators =
    match part with
    | P1 -> [ ( + ); ( * ) ]
    | P2 -> [ ( + ); ( * ); ( || ) ]
  in
  parse_equation lines
  |> List.fold ~init:0 ~f:(fun sum (res, nums) ->
    let rec try_all_possible_combinations = function
      | [] -> false
      | [ num ] -> Int.equal res num
      | n :: n' :: nums ->
        List.exists operators ~f:(fun op ->
          try_all_possible_combinations (op n n' :: nums))
    in
    if try_all_possible_combinations nums then sum + res else sum)
;;
