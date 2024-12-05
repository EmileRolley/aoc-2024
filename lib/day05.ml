open Core
open Day

let get_input = function
  | _ -> "inputs/day05/input.txt"
;;

module OrderingMap = Map.Make (String)

let parse_orderings_rules_and_updates =
  List.fold ~init:(OrderingMap.empty, []) ~f:(fun (rules, updates) line ->
    if String.contains line ','
    then rules, String.split_on_chars ~on:[ ',' ] line :: updates
    else (
      match Utils.scan "%d|%d" (fun a b -> a, b) line with
      | Some (a, b) ->
        ( Map.update rules (Int.to_string a) ~f:(function
            | Some v -> Int.to_string b :: v
            | None -> [ Int.to_string b ])
        , updates )
      | None -> rules, updates))
;;

let part_1 lines =
  let rules, updates = parse_orderings_rules_and_updates lines in
  let is_update_invalid update =
    List.existsi update ~f:(fun i n ->
      List.slice update 0 (i + 1)
      |> List.exists ~f:(fun n' ->
        match Map.find rules n with
        | None -> false
        | Some afters -> List.mem afters n' ~equal:String.equal))
  in
  List.fold updates ~init:0 ~f:(fun acc update ->
    if is_update_invalid update
    then acc
    else acc + Int.of_string (Utils.get_center_elem update))
;;

let part_2 _ = 0

let part p =
  match p with
  | P1 -> part_1
  | P2 -> part_2
;;
