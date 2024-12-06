open Core
open Day

let get_input = function
  | _ -> "inputs/day05/input.txt"
;;

module OrderMap = Map.Make (String)

let parse_order_and_updates =
  List.fold ~init:(OrderMap.empty, []) ~f:(fun (order, updates) line ->
    if String.contains line ','
    then order, String.split_on_chars ~on:[ ',' ] line :: updates
    else (
      match Utils.scan "%d|%d" (fun a b -> a, b) line with
      | Some (a, b) ->
        ( Map.update order (Int.to_string a) ~f:(function
            | Some afters_a -> Int.to_string b :: afters_a
            | None -> [ Int.to_string b ])
        , updates )
      | None -> order, updates))
;;

let part p lines =
  let order, updates = parse_order_and_updates lines in
  let is_update_invalid update =
    List.existsi update ~f:(fun i n ->
      List.slice update 0 (i + 1)
      |> List.exists ~f:(fun n' ->
        match Map.find order n with
        | None -> false
        | Some afters -> List.mem afters n' ~equal:String.equal))
  in
  List.fold updates ~init:0 ~f:(fun acc update ->
    (match p with
     | P1 ->
       (* Part 1: filter out invalid updates *)
       if is_update_invalid update then None else Some update
     | P2 ->
       (* Part 2: return the fixed invalid updates *)
       if is_update_invalid update
       then
         Some
           (List.sort update ~compare:(fun a b ->
              match Map.find order a, Map.find order b with
              | Some afters_a, _ ->
                if List.mem afters_a b ~equal:String.equal then -1 else 1
              | _, Some afters_b ->
                if List.mem afters_b a ~equal:String.equal then 1 else -1
              | _ -> 0))
       else None)
    |> Option.map ~f:(fun valid_update ->
      acc + Int.of_string (Utils.get_center_elem valid_update))
    |> Option.value ~default:acc)
;;
