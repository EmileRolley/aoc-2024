open Core
open Day

let get_input part =
  match part with
  | P1 -> "inputs/day01/part1.txt"
  | P2 -> "inputs/day01/part2.txt"
;;

let part_1 lines =
  let l, r =
    List.map
      ~f:
        (Utils.try_parse
           [ Utils.parse "%d   %d" (fun a b -> a, b); Utils.parse "" (0, 0) ])
      lines
    |> List.unzip
  in
  List.zip_exn (List.sort ~compare:Int.compare l) (List.sort ~compare:Int.compare r)
  |> List.fold_left ~init:0 ~f:(fun sum (l, r) -> sum + Int.abs (l - r))
  |> Int.to_string
;;

let part part lines =
  match part with
  | P1 -> part_1 lines
  | P2 -> "Not implemented"
;;
