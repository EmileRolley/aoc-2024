open Core

module type S = sig
  val run : unit -> unit
end

module Make (D : Aoc.Day.DAY) : S = struct
  let run () =
    let _ =
      [ P1; P2 ]
      |> List.map ~f:(fun p ->
        D.get_input p
        |> Aoc.Utils.read_input
        |> D.part p
        |> Printf.printf "Part %d: %s\n" (Aoc.Day.part_to_int p))
    in
    ()
  ;;
end
