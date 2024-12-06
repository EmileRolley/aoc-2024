open Day
open Core

let get_input = function
  | _ -> "inputs/day06/input.txt"
;;

type guard =
  { mutable position : int * int
  ; mutable facing : [ `Up | `Down | `Left | `Right ]
  }

let turn_right = function
  | `Up -> `Right
  | `Right -> `Down
  | `Down -> `Left
  | `Left -> `Up
;;

let position_compare (x, y) (x', y') =
  match Int.compare y y' with
  | 0 -> Int.compare x x'
  | c -> c
;;

let position_equal (x, y) (x', y') = x = x' && y = y'

let parse_map =
  List.foldi
    ~init:([], { position = 0, 0; facing = `Up })
    ~f:(fun y acc ->
      String.foldi ~init:acc ~f:(fun x (obstructions, guard) c ->
        match c with
        | '#' -> (x, y) :: obstructions, guard
        | '^' -> obstructions, { guard with position = x, y }
        | _ -> obstructions, guard))
;;

let part_1 lines =
  let map_h = List.length lines in
  let map_w = String.length (List.hd_exn lines) in
  let is_in_map (x, y) = x >= 0 && y >= 0 && x < map_w && y < map_h in
  let obstructions, guard = parse_map lines in
  let visited_positions = ref [ guard.position ] in
  while is_in_map guard.position do
    if List.mem obstructions guard.position ~equal:position_equal
    then (
      visited_positions := List.drop !visited_positions 1;
      guard.position
      <- (match guard.position, guard.facing with
          | (x, y), `Up -> x, y + 1
          | (x, y), `Down -> x, y - 1
          | (x, y), `Left -> x + 1, y
          | (x, y), `Right -> x - 1, y);
      guard.facing <- turn_right guard.facing)
    else (
      guard.position
      <- (match guard.position, guard.facing with
          | (x, y), `Up -> x, y - 1
          | (x, y), `Down -> x, y + 1
          | (x, y), `Left -> x - 1, y
          | (x, y), `Right -> x + 1, y);
      visited_positions := guard.position :: !visited_positions)
  done;
  (!visited_positions |> List.stable_dedup ~compare:position_compare |> List.length) - 1
;;

let part_2 _lines = 0

let part = function
  | P1 -> part_1
  | P2 -> part_2
;;
