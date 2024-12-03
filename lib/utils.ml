open Core

type part =
  | Example
  | Puzzle

type day =
  | DayOne of part
  | DayTwo of part
  | DayThree of part

let get_puzzle_file day =
  let dir_name = "puzzles/" in
  let file_name =
    match day with
    | DayOne Example -> "day1/example.txt"
    | DayOne Puzzle -> "day1/puzzle.txt"
    | DayTwo Example -> "day2/example.txt"
    | DayTwo Puzzle -> "day2/puzzle.txt"
    | DayThree Example -> "day3/example.txt"
    | DayThree Puzzle -> "day3/puzzle.txt"
  in
  dir_name ^ file_name
;;

let get_puzzle day =
  let file = get_puzzle_file day in
  Stdio.In_channel.with_file file ~f:(fun channel -> In_channel.input_all channel)
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
