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
  if equal_int int Int.max_value
  then failwith "Too big"
  else (
    let multiplied = int * 2024 in
    string_of_int multiplied)
;;

let remove_leading_zero num =
  match num with
  | [] -> []
  | [ zero ] -> [ zero ]
  | h :: t -> if equal_char '0' h then t else h :: t
;;

let remove_leading_zeroes num =
  String.to_list num
  |> List.fold_until
       ~init:([], false)
       ~f:(fun (chars, has_zero) curr ->
         match curr with
         | '0' ->
           if has_zero
           then Stop (String.of_char_list chars)
           else (
             match chars with
             | [] -> Continue ([ curr ], true)
             | _ -> Continue (chars @ [ curr ], true))
         | c -> Continue (chars @ [ c ], has_zero))
       ~finish:(fun (chars, _) -> remove_leading_zero chars |> String.of_char_list)
;;

let split_into_two num =
  let len = String.length num in
  let half = len / 2 in
  let first = String.slice num 0 half in
  let second = String.slice num half len |> remove_leading_zeroes in
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
  let formatted = String.concat nums ~sep:", " in
  Printf.printf "\nBlinks: %d | Num nums: %s\n" iterations_left formatted;
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
