open Utils
open Core

let is_safe lines =
  List.fold_until
    lines
    ~init:(None, None, 0)
    ~f:(fun (prev, direction, num_bad) curr ->
      match prev with
      | None ->
        (* First element: no previous value to compare *)
        Continue (Some curr, None, num_bad)
      | Some prev_val ->
        let diff = abs (curr - prev_val) in
        (* Any two adjacent levels differ by at least one and at most three. *)
        if diff < 1 || diff > 3
        then
          if equal_int 1 num_bad
          then
            (* The difference is invalid and we've reached the maximum "bad" levels, terminate early. *)
            Stop false
          else
            (* The difference is invalid, drop the current level, increment the "bad" levels and continue. *)
            Continue (prev, direction, num_bad + 1)
        else (
          let new_direction =
            if curr > prev_val
            then Some `Increasing
            else if curr < prev_val
            then Some `Decreasing
            else direction (* No change if the numbers are equal *)
          in
          match direction, new_direction with
          | Some `Increasing, Some `Decreasing ->
            if equal_int 1 num_bad
            then Stop false
            else Continue (prev, direction, num_bad + 1)
          | Some `Decreasing, Some `Increasing ->
            if equal_int 1 num_bad
            then Stop false
            else Continue (prev, direction, num_bad + 1)
          | _ -> Continue (Some curr, new_direction, num_bad)))
    ~finish:(fun (_, _, num_bad) -> num_bad <= 1)
;;

let () =
  let day = DayTwo Puzzle in
  let reports = get_puzzle_lines_as_nums day in
  let valid_reports = List.count reports ~f:is_safe in
  print_endline (Int.to_string valid_reports)
;;
