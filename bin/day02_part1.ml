open Utils
open Core

let is_safe lines =
  List.fold_until
    lines
    ~init:(None, None, true)
    ~f:(fun (prev, direction, valid) curr ->
      match prev with
      | None ->
        (* First element: no previous value to compare *)
        Continue (Some curr, None, valid)
      | Some prev_val ->
        let diff = abs (curr - prev_val) in
        (* Any two adjacent levels differ by at least one and at most three. *)
        if diff < 1 || diff > 3
        then (* The difference is invalid, terminate early. *)
          Stop false
        else (
          let new_direction =
            if curr > prev_val
            then Some `Increasing
            else if curr < prev_val
            then Some `Decreasing
            else direction (* No change if the numbers are equal *)
          in
          match direction, new_direction with
          | Some `Increasing, Some `Decreasing -> Stop false
          | Some `Decreasing, Some `Increasing -> Stop false
          | _ -> Continue (Some curr, new_direction, valid)))
    ~finish:(fun (_, _, valid) -> valid)
;;

let () =
  let day = DayTwo Puzzle in
  let reports = get_puzzle_lines_as_nums day in
  let valid_reports = List.count reports ~f:is_safe in
  print_endline (Int.to_string valid_reports)
;;
