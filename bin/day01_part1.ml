open Utils
open Core

let () =
  let day = DayOne Puzzle in
  let lines = get_puzzle_lines day in
  let nums =
    List.map lines ~f:(fun line ->
      String.split line ~on:' ' |> List.filter_map ~f:Int.of_string_opt)
  in
  let left =
    List.map nums ~f:(function
      | [ x; _ ] -> x
      | _ -> failwith "Expected two elements")
    |> List.sort ~compare
  in
  let right =
    List.map nums ~f:(function
      | [ _; y ] -> y
      | _ -> failwith "Expected two elements")
    |> List.sort ~compare
  in
  let distances = List.map2_exn left right ~f:(fun x y -> x - y |> Int.abs) in
  let sum = List.fold distances ~init:0 ~f:(fun a b -> a + b) |> Int.to_string in
  print_endline sum
;;
