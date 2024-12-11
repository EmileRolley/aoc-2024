open Core
module Day01 = Runnable.Make (Aoc.Day01)
module Day02 = Runnable.Make (Aoc.Day02)
module Day03 = Runnable.Make (Aoc.Day03)
module Day04 = Runnable.Make (Aoc.Day04)
module Day05 = Runnable.Make (Aoc.Day05)
module Day06 = Runnable.Make (Aoc.Day06)
module Day07 = Runnable.Make (Aoc.Day07)
module Day08 = Runnable.Make (Aoc.Day08)
module Day09 = Runnable.Make (Aoc.Day09)
module Day10 = Runnable.Make (Aoc.Day10)
module Day11 = Runnable.Make (Aoc.Day11)

let () =
  let args = Sys.get_argv () in
  let nb_args = Array.length args in
  if nb_args < 2
  then Printf.printf "Usage: %s <num_day> [<num_part>]\n" args.(0)
  else (
    let part =
      if Int.equal nb_args 3 then Some (Aoc.Day.part_of_string args.(2)) else None
    in
    match args.(1) with
    | "1" -> Day01.run part
    | "2" -> Day02.run part
    | "3" -> Day03.run part
    | "4" -> Day04.run part
    | "5" -> Day05.run part
    | "6" -> Day06.run part
    | "7" -> Day07.run part
    | "8" -> Day08.run part
    | "9" -> Day09.run part
    | "10" -> Day10.run part
    | "11" -> Day11.run part
    | _ -> Printf.printf "No such day\n")
;;
