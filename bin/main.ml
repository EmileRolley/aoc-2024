open Core
module Day01 = Runnable.Make (Aoc.Day01)

let () =
  let args = Sys.get_argv () in
  if Array.length args < 2
  then Printf.printf "Usage: %s <num_day>\n" args.(0)
  else (
    match args.(1) with
    | "01" | "1" -> Day01.run ()
    | _ -> Printf.printf "No such day\n")
;;
