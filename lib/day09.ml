open Core

let get_input = function
  | _ -> "inputs/day09/input.txt"
;;

type memory =
  { payload : string array
  ; mutable size : int
  }

let parse_memory raw =
  let payload =
    String.foldi raw ~init:[] ~f:(fun i acc c ->
      let file_id = Int.to_string (i / 2) in
      let n = Int.of_string (String.of_char c) in
      acc @ List.init n ~f:(fun _ -> if i mod 2 = 1 then "." else file_id))
  in
  { payload = Array.of_list payload; size = List.length payload }
;;

let is_free_space = String.equal "."

let get_new_size mem =
  let size = ref (mem.size - 1) in
  while is_free_space mem.payload.(!size - 1) do
    size := !size - 1
  done;
  !size
;;

let compact_memory m =
  Array.foldi m.payload ~init:m ~f:(fun i mem s ->
    if is_free_space s && i < mem.size
    then (
      mem.payload.(i) <- mem.payload.(mem.size - 1);
      mem.size <- get_new_size mem;
      mem)
    else mem)
;;

let compute_checksum mem =
  let res = ref 0 in
  for i = 0 to mem.size - 1 do
    let id = Int.of_string mem.payload.(i) in
    res := !res + (i * id)
  done;
  !res
;;

let part part lines =
  match part with
  | Day.P1 -> List.hd_exn lines |> parse_memory |> compact_memory |> compute_checksum
  | Day.P2 -> 0
;;
