open Core

type part =
  | Example
  | Puzzle

type day =
  | DayOne of part
  | DayTwo of part
  | DayThree of part
  | DayFour of part
  | DayFive of part
  | DayEleven of part

let get_puzzle_file day =
  let dir_name = "puzzles/" in
  let file_name =
    match day with
    | DayOne Example -> "day01/example.txt"
    | DayOne Puzzle -> "day01/puzzle.txt"
    | DayTwo Example -> "day02/example.txt"
    | DayTwo Puzzle -> "day02/puzzle.txt"
    | DayThree Example -> "day03/example.txt"
    | DayThree Puzzle -> "day03/puzzle.txt"
    | DayFour Example -> "day04/example.txt"
    | DayFour Puzzle -> "day04/puzzle.txt"
    | DayFive Example -> "day05/example.txt"
    | DayFive Puzzle -> "day05/puzzle.txt"
    | DayEleven Example -> "day11/example.txt"
    | DayEleven Puzzle -> "day11/puzzle.txt"
  in
  dir_name ^ file_name
;;

let get_puzzle day =
  let file = get_puzzle_file day in
  Stdio.In_channel.with_file file ~f:(fun channel -> In_channel.input_all channel)
  |> String.strip
;;

let get_puzzle_lines day =
  let puzzle = get_puzzle day in
  String.split_lines puzzle
;;

let get_puzzle_lines_as_nums day =
  let puzzle = get_puzzle day in
  String.split_lines puzzle
  |> List.map ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
;;

let get_puzzle_char_grid day =
  let puzzle = get_puzzle day in
  String.split_lines puzzle
  |> Array.of_list_map ~f:(fun s -> Array.of_list (String.to_list s))
;;
