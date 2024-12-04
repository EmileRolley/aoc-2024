open Core
open Day

let get_input = function
  | _ -> "inputs/day04/input.txt"
;;

let one_if_equal w = if String.equal w "XMAS" || String.equal w "SAMX" then 1 else 0

let part_1 lines =
  let rows = Array.of_list lines in
  let get_col_slice j start stop =
    Array.filter_mapi rows ~f:(fun k row ->
      if k >= start && k < stop then Some row.[j] else None)
    |> String.of_array
  in
  let check_right row j () =
    let word = String.slice row j (j + 4) in
    one_if_equal word
  in
  let check_down i j () =
    let word = get_col_slice j i (i + 4) in
    one_if_equal word
  in
  let check_bottom_right_diag i j () =
    let word =
      String.of_list
        [ rows.(i).[j]; rows.(i + 1).[j + 1]; rows.(i + 2).[j + 2]; rows.(i + 3).[j + 3] ]
    in
    one_if_equal word
  in
  let check_bottom_left_diag i j () =
    let word =
      String.of_list
        [ rows.(i).[j]; rows.(i + 1).[j - 1]; rows.(i + 2).[j - 2]; rows.(i + 3).[j - 3] ]
    in
    one_if_equal word
  in
  let nb_row = Array.length rows in
  Array.foldi rows ~init:0 ~f:(fun i acc row ->
    let nb_col = String.length row in
    String.foldi row ~init:acc ~f:(fun j acc' _ ->
      match row.[j] with
      | 'X' | 'S' ->
        acc'
        + Utils.cond_apply_and_sum_all
            [ j + 3 < nb_col, check_right row j
            ; i + 3 < nb_row, check_down i j
            ; i + 3 < nb_row && j + 3 < nb_col, check_bottom_right_diag i j
            ; i + 3 < nb_row && 2 < j, check_bottom_left_diag i j
            ]
      | _ -> acc'))
;;

let part p lines =
  match p with
  | P1 -> part_1 lines
  | P2 -> 0
;;
