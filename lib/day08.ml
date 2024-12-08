open Day
open Core

let get_input = function
  | _ -> "inputs/day08/input.txt"
;;

module AntenasMap = Map.Make (Char)

let parse_antenas =
  List.foldi ~init:AntenasMap.empty ~f:(fun x acc line ->
    String.foldi line ~init:acc ~f:(fun y acc ->
      function
      | '.' -> acc
      | c ->
        Map.update acc c ~f:(function
          | Some l -> (x, y) :: l
          | None -> [ x, y ])))
;;

let part part lines =
  let height = List.length lines in
  let width = String.length (List.hd_exn lines) in
  let is_in_bounds (x, y) = x >= 0 && x < width && y >= 0 && y < height in
  let get_antinodes_for (ax, ay) (ax', ay') =
    let diff_x = ax - ax' in
    let diff_y = ay - ay' in
    match part with
    | P1 -> [ ax + diff_x, ay + diff_y; ax' - diff_x, ay' - diff_y ]
    | P2 ->
      let res = ref [] in
      for i = 0 to height - 1 do
        res
        := ((diff_x * i) + ax, (diff_y * i) + ay)
           :: ((-diff_x * i) + ax, (-diff_y * i) + ay)
           :: !res
      done;
      !res
  in
  let rec get_antinodes acc = function
    | [] -> acc
    | a :: rest ->
      let res =
        List.fold rest ~init:acc ~f:(fun res a' -> get_antinodes_for a a' :: res)
      in
      get_antinodes res rest
  in
  parse_antenas lines
  |> Map.fold ~init:[] ~f:(fun ~key:_ ~data res -> get_antinodes res data)
  |> List.join
  |> List.stable_dedup ~compare:Utils.position_compare
  |> List.filter ~f:is_in_bounds
  |> List.length
;;
