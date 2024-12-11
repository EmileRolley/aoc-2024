open Core

let get_input = function
  | _ -> "inputs/day09/test.txt"
;;

let is_free_space = String.equal "."

type memory_part_1 =
  { payload : string array
  ; mutable size : int
  }

let part_1 line =
  let parse_memory raw =
    let payload =
      String.foldi raw ~init:[] ~f:(fun i acc c ->
        let file_id = Int.to_string (i / 2) in
        let n = Int.of_string (String.of_char c) in
        acc @ List.init n ~f:(fun _ -> if i mod 2 = 1 then "." else file_id))
    in
    { payload = Array.of_list payload; size = List.length payload }
  in
  let get_new_size mem =
    let size = ref (mem.size - 1) in
    while is_free_space mem.payload.(!size - 1) do
      size := !size - 1
    done;
    !size
  in
  let compact_memory m =
    Array.foldi m.payload ~init:m ~f:(fun i mem s ->
      if is_free_space s && i < mem.size
      then (
        mem.payload.(i) <- mem.payload.(mem.size - 1);
        mem.size <- get_new_size mem;
        mem)
      else mem)
  in
  let compute_checksum mem =
    let res = ref 0 in
    for i = 0 to mem.size - 1 do
      let id = Int.of_string mem.payload.(i) in
      res := !res + (i * id)
    done;
    !res
  in
  parse_memory line |> compact_memory |> compute_checksum
;;

type file =
  { id : string
  ; range : int * int
  ; size : int
  }

type memory_part_2 =
  { files : file list
  ; free_spaces : (int * int) list
  }

let part_2 line =
  let parse_memory raw =
    let memory, _ =
      String.foldi
        raw
        ~init:({ files = []; free_spaces = [] }, 0)
        ~f:(fun i (mem, v_idx) c ->
          let id = Int.to_string (i / 2) in
          let n = Int.of_string (String.of_char c) in
          let range = v_idx, v_idx + n in
          if i mod 2 = 1
          then { mem with free_spaces = range :: mem.free_spaces }, v_idx + n
          else { mem with files = { id; range; size = n } :: mem.files }, v_idx + n)
    in
    memory
  in
  let compact_memory mem =
    let free_spaces, filled_spaces =
      List.fold
        mem.files
        ~init:(List.rev mem.free_spaces, [])
        ~f:(fun (free, res) file ->
          let free = List.sort free ~compare:Utils.position_compare in
          match List.findi free ~f:(fun _ (start, stop) -> file.size <= stop - start) with
          | Some (i, (free_start, free_stop)) when free_start < fst file.range ->
            if free_stop - free_start = file.size
            then
              ( file.range :: Utils.List.remove free ~at:i
              , (file.id, (free_start, free_stop)) :: res )
            else (
              let replaced =
                Utils.List.replace free ~at:i ~e:(free_start + file.size, free_stop)
              in
              ( file.range :: replaced
              , (file.id, (free_start, free_start + file.size)) :: res ))
          | _ -> free, (file.id, file.range) :: res)
    in
    let free_spaces = List.map free_spaces ~f:(fun fs -> ".", fs) in
    List.sort (free_spaces @ filled_spaces) ~compare:(fun (_, range) (_, range') ->
      Utils.position_compare range range')
  in
  let compute_checksum mem =
    let compute id start stop =
      let res = ref 0 in
      for i = start to stop - 1 do
        res := !res + (i * id)
      done;
      !res
    in
    let check_sum, _ =
      List.fold mem ~init:(0, 0) ~f:(fun (check_sum, idx) (id, (s, s')) ->
        let range_size = s' - s in
        ( (if is_free_space id
           then check_sum
           else check_sum + compute (Int.of_string id) idx (idx + range_size))
        , idx + range_size ))
    in
    check_sum
  in
  parse_memory line |> compact_memory |> compute_checksum
;;

let part part lines =
  match part with
  | Day.P1 -> List.hd_exn lines |> part_1
  | Day.P2 -> List.hd_exn lines |> part_2
;;
