type part = P1 | P2

module type DAY = sig
  val get_input : part -> string
  val part : part -> string list -> string
end
