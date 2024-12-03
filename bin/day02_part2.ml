open Utils
open Core

let is_valid_list lines =
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
            if curr > prev_val then Some `Increasing else Some `Decreasing
          in
          match direction, new_direction with
          | Some `Increasing, Some `Decreasing -> Stop false
          | Some `Decreasing, Some `Increasing -> Stop false
          | _ -> Continue (Some curr, new_direction, valid)))
    ~finish:(fun (_, _, valid) -> valid)
;;

let rec remove_element lst idx =
  match lst, idx with
  (* If the list is empty, it doesn't matter what the provided index is: just return the empty list. *)
  | [], _ -> []
  (* If the provided index is 0, remove the current element at the head of the list and return the tail. *)
  | _ :: tl, 0 -> tl
  (* If the provided index is any other number, return the list with the next element of the tail removed. *)
  | hd :: tl, n -> hd :: remove_element tl (n - 1)
;;

let is_safe lst =
  if is_valid_list lst
  then (* The list is valid without counting *)
    true
  else (
    (* Let's get recursive. *)
    let rec check_removals i =
      (* Base case: We've reached the end of the list, it's invalid. *)
      if i >= List.length lst
      then false
      else (
        (* Remove the next element from the list and check if the new list is valid *)
        let new_lst = remove_element lst i in
        if is_valid_list new_lst
        then true
        else (* The new list isn't valid, recurse. *)
          check_removals (i + 1))
    in
    check_removals 0)
;;

let () =
  let day = DayTwo Puzzle in
  let reports = get_puzzle_lines_as_nums day in
  let valid_reports = List.count reports ~f:is_safe in
  print_endline (Int.to_string valid_reports)
;;
