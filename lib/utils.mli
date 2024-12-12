type part =
  | Example
  | Puzzle

type day =
  | DayOne of part
  | DayTwo of part
  | DayThree of part
  | DayFour of part
  | DayEleven of part

val get_puzzle : day -> string
val get_puzzle_lines : day -> string list
val get_puzzle_lines_as_nums : day -> int list list
val get_puzzle_char_grid : day -> char array array
