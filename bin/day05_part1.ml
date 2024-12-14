open Core
open Utils

let parse puzzle =
  let part1_lines, part2_lines =
    match List.split_while puzzle ~f:(fun line -> not (String.is_empty line)) with
    | part1, _ :: part2 -> part1, part2
    | part1, [] -> part1, []
  in
  let orderings =
    List.map part1_lines ~f:(fun line ->
      match String.split line ~on:'|' with
      | [ a; b ] -> Int.of_string (String.strip a), Int.of_string (String.strip b)
      | _ -> failwithf "Invalid format in Ordering line: %s" line ())
  in
  let operations =
    List.map part2_lines ~f:(fun line ->
      List.map (String.split line ~on:',') ~f:(fun num ->
        Int.of_string (String.strip num)))
  in
  orderings, operations
;;

let create_adjacency_list edges =
  List.fold edges ~init:Int.Map.empty ~f:(fun acc (src, dest) ->
    Map.update acc src ~f:(function
      | None -> [ dest ]
      | Some neighbors -> dest :: neighbors))
;;

let is_path_valid adjacency_list path =
  let edge_exists src dest =
    match Map.find adjacency_list src with
    | None -> false
    | Some neighbors -> List.mem neighbors dest ~equal:equal_int
  in
  let rec check_pairs = function
    | [] | [ _ ] -> true
    | src :: dest :: rest ->
      if edge_exists src dest then check_pairs (dest :: rest) else false
  in
  check_pairs path
;;

let middle lst =
  let len = List.length lst in
  len / 2
;;

let () =
  let day = DayFive Puzzle in
  let puzzle = get_puzzle_lines day in
  let orderings, paths = parse puzzle in
  let adjacency_list = create_adjacency_list orderings in
  let middles =
    List.filter_map paths ~f:(fun path ->
      if is_path_valid adjacency_list path
      then Some (List.nth_exn path (middle path))
      else None)
  in
  let sums = List.fold middles ~init:0 ~f:(fun acc n -> acc + n) in
  print_endline (string_of_int sums)
;;
