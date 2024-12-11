type part =
  | P1
  | P2

let part_to_int = function
  | P1 -> 1
  | P2 -> 2
;;

let part_of_string = function
  | "1" -> P1
  | "2" -> P2
  | part -> failwith ("Invalid part: " ^ part ^ ". Valid parts are 1 and 2.")
;;

module type DAY = sig
  val get_input : part -> string
  val part : part -> string list -> int
end
