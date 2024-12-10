open Core

let get_input = function
  | _ -> "inputs/day10/test.txt"
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

let get_plus_one_neighbors map (x, y) elem =
  Utils.Matrix.fold_in
    map
    ~init:[]
    ~start:(x - 1, y - 1)
    ~stop:(x + 2, y + 2)
    ~f:(fun acc ~pos ~e ->
      if not_in_diag pos (x, y) && e = elem + 1 then pos :: acc else acc)
;;

let get_score map (x, y) elem =
  let rec aux acc (x, y) elem =
    if elem = 9
    then (x, y) :: acc
    else
      get_plus_one_neighbors map (x, y) elem
      |> List.fold_left ~init:acc ~f:(fun acc pos -> aux acc pos (elem + 1))
  in
  aux [] (x, y) elem |> List.stable_dedup ~compare:Utils.position_compare |> List.length
;;
let part part lines =
  let map = parse_map lines in
  match part with
  | Day.P1 ->
    Array.foldi map ~init:0 ~f:(fun i acc row ->
      Array.foldi row ~init:acc ~f:(fun j acc elem ->
        if Int.equal elem 0 then acc + get_score map (i, j) elem else acc))
  | Day.P2 -> 0
;;
