module type Runnable = sig
  val run : unit -> unit
end

module Make (D : Aoc.Day.DAY) : Runnable = struct
  (** Read input from a file *)
  let read_input input =
    let file = open_in input in
    let rec read_lines acc =
      try
        let line = input_line file in
        read_lines (line :: acc)
      with End_of_file -> List.rev acc
    in
    let lines = read_lines [] in
    close_in file;
    lines

  let run () =
    let part1 = D.get_input P1 |> read_input |> D.part P1 in
    (* let part2 = D.part_2 input in *)
    Printf.printf "Part 1: %s\n" part1
  (* Printf.printf "Part 2: %s\n" part2 *)
end

module Runnable_Day01 = Make (Aoc.Day01)

let () =
  let args = Sys.argv in
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <num_day>\n" args.(0)
  else
    match args.(1) with
    | "01" | "1" -> Runnable_Day01.run ()
    | _ -> Printf.printf "No such day\n"
