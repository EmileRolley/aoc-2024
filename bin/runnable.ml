open Core

module type S = sig
  val run : Aoc.Day.part option -> unit
end

module Make (D : Aoc.Day.DAY) : S = struct
  let run part =
    let parts =
      match part with
      | Some p -> [ p ]
      | None -> [ Aoc.Day.P1; Aoc.Day.P2 ]
    in
    List.map parts ~f:(fun p ->
      D.get_input p
      |> Aoc.Utils.read_input
      |> D.part p
      |> Printf.printf "Part %d: %d\n" (Aoc.Day.part_to_int p))
    |> ignore
  ;;
end
