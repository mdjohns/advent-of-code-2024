open Core
open Utils

type rule =
  | ReplaceWithOne
  | SplitIntoTwo
  | MultiplyBy2024

let is_length_even num =
  let len = String.length num in
  let modulo = len % 2 in
  equal_int 0 modulo
;;

let multiply_by_2024 num =
  let int = int_of_string num in
  let multiplied = 2024 * int in
  string_of_int multiplied
;;

let rec remove_leading_zero nums =
  let len = List.length nums in
  match nums with
  | [] -> []
  | one_item when equal_int 1 len -> one_item
  | h :: t -> if equal_char '0' h then remove_leading_zero t else h :: t
;;

let split_into_two num =
  let len = String.length num in
  let half = len / 2 in
  let first = String.slice num 0 half in
  let second =
    String.slice num half len
    |> String.to_list
    |> remove_leading_zero
    |> String.of_char_list
  in
  [ first ] @ [ second ]
;;

let get_rule num =
  match num with
  | "0" -> ReplaceWithOne
  | _ when is_length_even num -> SplitIntoTwo
  | _ -> MultiplyBy2024
;;

let process_rule num rule =
  match rule with
  | ReplaceWithOne -> [ "1" ]
  | MultiplyBy2024 -> [ multiply_by_2024 num ]
  | SplitIntoTwo -> split_into_two num
;;

let rec process_puzzle nums iterations_left =
  if equal_int 0 iterations_left
  then nums
  else (
    let new_iterations = iterations_left - 1 in
    let new_nums =
      List.map nums ~f:(fun n ->
        let rule = get_rule n in
        process_rule n rule)
      |> List.concat
    in
    process_puzzle new_nums new_iterations)
;;

let () =
  let day = DayEleven Puzzle in
  let puzzle = get_puzzle day |> String.split ~on:' ' in
  let result = process_puzzle puzzle 25 |> List.length |> string_of_int in
  print_endline result
;;
