open Core

let get_input = function
  | _ -> "inputs/day10/input.txt"
;;

let parse_map lines =
  let map =
    Utils.Matrix.make
      ~dimx:(List.length lines)
      ~dimy:(String.length (List.hd_exn lines))
      (-1)
  in
  List.iteri lines ~f:(fun i line ->
    String.iteri line ~f:(fun j c -> map.(i).(j) <- Int.of_string @@ String.of_char c));
  map
;;

let not_in_diag (x, y) (x', y') = x = x' || y = y'

let get_all_accessed_summits map (x, y) num =
  let get_plus_one_neighbors (x, y) num =
    Utils.Matrix.fold_in
      map
      ~init:[]
      ~start:(x - 1, y - 1)
      ~stop:(x + 2, y + 2)
      ~f:(fun acc ~pos ~e ->
        if not_in_diag pos (x, y) && e = num + 1 then pos :: acc else acc)
  in
  let rec aux acc (x, y) num =
    if num = 9
    then (x, y) :: acc
    else
      get_plus_one_neighbors (x, y) num
      |> List.fold_left ~init:acc ~f:(fun acc pos -> aux acc pos (num + 1))
  in
  aux [] (x, y) num
;;

let part part lines =
  let map = parse_map lines in
  Array.foldi map ~init:0 ~f:(fun i acc row ->
    Array.foldi row ~init:acc ~f:(fun j acc num ->
      if Int.equal num 0
      then (
        let summits = get_all_accessed_summits map (i, j) num in
        acc
        + List.length
            (match part with
             | Day.P1 -> List.stable_dedup summits ~compare:Utils.position_compare
             | Day.P2 -> summits))
      else acc))
;;
