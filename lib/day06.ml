open Day
open Core

let get_input = function
  | _ -> "inputs/day06/input.txt"
;;

type guard_position =
  { mutable pos : int * int
  ; mutable facing : [ `Up | `Down | `Left | `Right ]
  }

let position_equal (x, y) (x', y') = x = x' && y = y'

let guard_equal guard guard' =
  position_equal guard.pos guard'.pos
  &&
  match guard.facing, guard'.facing with
  | `Up, `Up -> true
  | `Down, `Down -> true
  | `Left, `Left -> true
  | `Right, `Right -> true
  | _ -> false
;;

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

let parse_map =
  List.foldi
    ~init:([], { pos = 0, 0; facing = `Up })
    ~f:(fun y acc ->
      String.foldi ~init:acc ~f:(fun x (obstructions, guard) c ->
        match c with
        | '#' -> (x, y) :: obstructions, guard
        | '^' -> obstructions, { guard with pos = x, y }
        | _ -> obstructions, guard))
;;

let walk ~until ~obstructions ~guard:guard_pos =
  let visited_ref = ref [] in
  let guard = { pos = guard_pos.pos; facing = guard_pos.facing } in
  let step dir guard =
    match dir, guard.pos, guard.facing with
    | `Forward, (x, y), `Up -> x, y - 1
    | `Backward, (x, y), `Up -> x, y + 1
    | `Forward, (x, y), `Down -> x, y + 1
    | `Backward, (x, y), `Down -> x, y - 1
    | `Forward, (x, y), `Left -> x - 1, y
    | `Backward, (x, y), `Left -> x + 1, y
    | `Forward, (x, y), `Right -> x + 1, y
    | `Backward, (x, y), `Right -> x - 1, y
  in
  while not @@ until !visited_ref guard do
    if List.mem obstructions guard.pos ~equal:position_equal
    then (
      visited_ref := List.drop !visited_ref 1;
      guard.pos <- step `Backward guard;
      guard.facing <- turn_right guard.facing)
    else (
      guard.pos <- step `Forward guard;
      visited_ref := { pos = guard.pos; facing = guard.facing } :: !visited_ref)
  done;
  !visited_ref
;;

let part_1 _ _ is_not_in_map obstructions guard =
  walk ~until:(fun _ guard -> is_not_in_map guard.pos) ~obstructions ~guard
  |> List.map ~f:(fun guard -> guard.pos)
  |> List.stable_dedup ~compare:position_compare
  |> List.length
  |> ( + ) Int.minus_one
;;

(* TODO: Provides the correct answer but it's the naive brut force algorithm
   which took 30 min. Should do the backtracking one at some point. *)
let part_2 map_h map_w is_not_in_map obstructions start_guard_pos =
  let res = ref 0 in
  for i = 0 to map_h - 1 do
    for j = 0 to map_w - 1 do
      let new_obstruction = j, i in
      if (not @@ List.mem obstructions new_obstruction ~equal:position_equal)
         && (not @@ position_equal start_guard_pos.pos new_obstruction)
      then
        walk
          ~until:(fun visited guard_pos ->
            let nb_visited = List.length visited in
            let visited_without_start_and_curr =
              if nb_visited < 2 then [] else List.slice visited 1 (nb_visited - 1)
            in
            if is_not_in_map guard_pos.pos
            then true
            else if List.mem visited_without_start_and_curr guard_pos ~equal:guard_equal
            then (
              res := !res + 1;
              true)
            else false)
          ~obstructions:(new_obstruction :: obstructions)
          ~guard:start_guard_pos
        |> ignore
      else ()
    done
  done;
  !res
;;

let part part lines =
  let map_h = List.length lines in
  let map_w = String.length (List.hd_exn lines) in
  let is_not_in_map (x, y) = x < 0 || y < 0 || x >= map_w || y >= map_h in
  let obstructions, start_guard_pos = parse_map lines in
  (match part with
   | P1 -> part_1
   | P2 -> part_2)
    map_h
    map_w
    is_not_in_map
    obstructions
    start_guard_pos
;;
