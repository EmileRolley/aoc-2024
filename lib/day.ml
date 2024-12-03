type part =
  | P1
  | P2

let part_to_int = function
  | P1 -> 1
  | P2 -> 2
;;

module type DAY = sig
  val get_input : part -> string
  val part : part -> string list -> string
end
