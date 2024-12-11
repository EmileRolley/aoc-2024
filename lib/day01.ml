open Core
open Day

let get_input _ = "inputs/day01/inputs.txt"

let parse_pairs =
  List.map
    ~f:
      (Utils.try_scan
         [ Utils.scan "%d   %d" (fun a b -> a, b); (* EOF *) Utils.scan "" (0, 0) ])
;;

let part_1 lines =
  let l, r = parse_pairs lines |> List.unzip in
  List.zip_exn (List.sort ~compare:Int.compare l) (List.sort ~compare:Int.compare r)
  |> Utils.List.sum ~f:(fun (l, r) -> Int.abs (l - r))
;;

let part_2 lines =
  let get_nb_occurences =
    List.fold
      ~init:(Map.empty (module Int))
      ~f:(fun acc x ->
        Map.change acc x ~f:(function
          | None -> Some 1
          | Some v -> Some (v + 1)))
  in
  let left, right = parse_pairs lines |> List.unzip in
  let left_occurences = get_nb_occurences left in
  let right_occurences = get_nb_occurences right in
  left_occurences
  |> Map.fold ~init:0 ~f:(fun ~key:num ~data:nb_occurences_in_left acc ->
    match Map.find right_occurences num with
    | None -> acc
    | Some nb_occurences_in_right ->
      (nb_occurences_in_left * (num * nb_occurences_in_right)) + acc)
;;

let part = function
  | P1 -> part_1
  | P2 -> part_2
;;
