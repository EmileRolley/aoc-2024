open Core
open Day

let get_input = function
  | _ -> "inputs/day04/input.txt"
;;

let part_1 lines =
  let rows = Array.of_list lines in
  let one_if_equal w = if String.equal w "XMAS" || String.equal w "SAMX" then 1 else 0 in
  let get_col_slice j start stop =
    Array.filter_mapi rows ~f:(fun k row ->
      if k >= start && k < stop then Some row.[j] else None)
    |> String.of_array
  in
  let check_right row j () = one_if_equal @@ String.slice row j (j + 4) in
  let check_down i j () = one_if_equal @@ get_col_slice j i (i + 4) in
  let check_bottom_right_diag i j () =
    one_if_equal
    @@ String.of_list
         [ rows.(i).[j]
         ; rows.(i + 1).[j + 1]
         ; rows.(i + 2).[j + 2]
         ; rows.(i + 3).[j + 3]
         ]
  in
  let check_bottom_left_diag i j () =
    one_if_equal
    @@ String.of_list
         [ rows.(i).[j]
         ; rows.(i + 1).[j - 1]
         ; rows.(i + 2).[j - 2]
         ; rows.(i + 3).[j - 3]
         ]
  in
  let nb_row = Array.length rows in
  Array.foldi rows ~init:0 ~f:(fun i acc row ->
    let nb_col = String.length row in
    String.foldi row ~init:acc ~f:(fun j acc' _ ->
      match row.[j] with
      | 'X' | 'S' ->
        List.fold
          ~init:acc'
          ~f:(fun acc'' (cond, f) -> if cond then acc'' + f () else acc'')
          [ j + 3 < nb_col, check_right row j
          ; i + 3 < nb_row, check_down i j
          ; i + 3 < nb_row && j + 3 < nb_col, check_bottom_right_diag i j
          ; i + 3 < nb_row && 2 < j, check_bottom_left_diag i j
          ]
      | _ -> acc'))
;;

let part_2 lines =
  let rows = Array.of_list lines in
  let nb_row = Array.length rows in
  let equal_mas_or_sam w = String.equal w "MAS" || String.equal w "SAM" in
  Array.foldi rows ~init:0 ~f:(fun i acc row ->
    let nb_col = String.length row in
    String.foldi row ~init:acc ~f:(fun j acc' _ ->
      if Char.equal row.[j] 'A' && 0 < i && i < nb_row - 1 && 0 < j && j < nb_col - 1
      then (
        let first_diag =
          String.of_list [ rows.(i - 1).[j - 1]; rows.(i).[j]; rows.(i + 1).[j + 1] ]
        in
        let second_diag =
          String.of_list [ rows.(i - 1).[j + 1]; rows.(i).[j]; rows.(i + 1).[j - 1] ]
        in
        if equal_mas_or_sam first_diag && equal_mas_or_sam second_diag
        then acc' + 1
        else acc')
      else acc'))
;;

let part p =
  match p with
  | P1 -> part_1
  | P2 -> part_2
;;
