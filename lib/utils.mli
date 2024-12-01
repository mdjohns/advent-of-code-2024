type part =
  | Example
  | Puzzle

type day = DayOne of part

val get_puzzle : day -> string
val get_puzzle_lines : day -> string list
