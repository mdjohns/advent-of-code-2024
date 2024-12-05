open Core
open Utils

let word = "XMAS"
let word_len = String.length word
let should_count ch = Char.equal 'X' ch
let directions = [| -1, 0; 1, 0; 0, -1; 0, 1; -1, -1; -1, 1; 1, -1; 1, 1 |]

let in_bounds row col num_rows num_cols =
  row >= 0 && row < num_rows && col >= 0 && col < num_cols
;;

let search_from grid row col (row_dir, col_dir) rows cols =
  let rec check i =
    if i = word_len
    then true
    else (
      let new_row = row + (i * row_dir) in
      let new_col = col + (i * col_dir) in
      if in_bounds new_row new_col rows cols
         && Char.equal grid.(new_row).(new_col) word.[i]
      then check (i + 1)
      else false)
  in
  check 0
;;

let count_occurrences grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let count = ref 0 in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      if should_count grid.(row).(col)
      then
        Array.iter directions ~f:(fun direction ->
          if search_from grid row col direction rows cols then incr count)
    done
  done;
  !count
;;

let () =
  DayFour Puzzle
  |> get_puzzle_char_grid
  |> count_occurrences
  |> string_of_int
  |> print_endline
;;
