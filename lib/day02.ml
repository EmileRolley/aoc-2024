open Core
open Day

let get_input _ = "inputs/day02/inputs.txt"

let parse_reports =
  List.map ~f:(fun report ->
    String.split ~on:' ' report |> List.map ~f:Int.of_string |> Array.of_list)
;;

let report_is_safe levels =
  let sign = levels.(0) - levels.(1) |> Int.max (-1) |> Int.min 1 in
  Array.slice levels 1 0
  |> Array.for_alli ~f:(fun i level ->
    match sign * (levels.(i) - level) with
    | 1 | 2 | 3 -> true
    | _ -> false)
;;

let part_1 reports = parse_reports reports |> List.filter ~f:report_is_safe |> List.length

let part_2 reports =
  parse_reports reports
  |> List.filter ~f:(fun report ->
    report_is_safe report
    || Array.existsi report ~f:(fun i _ -> Utils.array_remove report i |> report_is_safe))
  |> List.length
;;

let part = function
  | P1 -> part_1
  | P2 -> part_2
;;
