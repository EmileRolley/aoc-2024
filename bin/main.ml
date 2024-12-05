open Core
module Day01 = Runnable.Make (Aoc.Day01)
module Day02 = Runnable.Make (Aoc.Day02)
module Day03 = Runnable.Make (Aoc.Day03)
module Day04 = Runnable.Make (Aoc.Day04)
module Day05 = Runnable.Make (Aoc.Day05)

let () =
  let args = Sys.get_argv () in
  if Array.length args < 2
  then Printf.printf "Usage: %s <num_day>\n" args.(0)
  else (
    match args.(1) with
    | "01" | "1" -> Day01.run ()
    | "02" | "2" -> Day02.run ()
    | "03" | "3" -> Day03.run ()
    | "04" | "4" -> Day04.run ()
    | "05" | "5" -> Day05.run ()
    | _ -> Printf.printf "No such day\n")
;;
