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

let edge_exists adjacency_list src dest =
  match Map.find adjacency_list src with
  | None -> false
  | Some neighbors -> List.mem neighbors dest ~equal:equal_int
;;

let is_path_valid adjacency_list path =
  let rec check_pairs = function
    | [] | [ _ ] -> true
    | src :: dest :: rest ->
      if edge_exists adjacency_list src dest then check_pairs (dest :: rest) else false
  in
  check_pairs path
;;

let middle lst =
  let len = List.length lst in
  List.nth_exn lst (len / 2)
;;

let correct_path adjacency_list path =
  let rec build_path remaining_path current_path =
    match remaining_path with
    | [] -> Some (List.rev current_path)
    | _ ->
      (match current_path with
       | [] ->
         List.find_map remaining_path ~f:(fun node ->
           build_path (List.filter remaining_path ~f:(( <> ) node)) [ node ])
       | last_node :: _ ->
         List.find_map remaining_path ~f:(fun node ->
           if edge_exists adjacency_list last_node node
           then
             build_path
               (List.filter remaining_path ~f:(( <> ) node))
               (node :: current_path)
           else None))
  in
  build_path path []
;;

let () =
  let day = DayFive Puzzle in
  let puzzle = get_puzzle_lines day in
  let orderings, paths = parse puzzle in
  let adjacency_list = create_adjacency_list orderings in
  List.filter paths ~f:(fun path -> not (is_path_valid adjacency_list path))
  |> List.filter_map ~f:(fun path ->
    match correct_path adjacency_list path with
    | Some corrected_path -> Some (middle corrected_path)
    | None -> None)
  |> List.fold ~init:0 ~f:(fun acc n -> acc + n)
  |> string_of_int
  |> print_endline
;;
