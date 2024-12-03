open Angstrom
open Core
open Utils

let comma = char ','
let open_paren = char '('
let close_paren = char ')'

let digit =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let parse_instruction =
  string "mul" *> open_paren *> digit
  >>= fun first_digit ->
  comma *> digit
  >>= fun second_digit ->
  close_paren *> return (int_of_string first_digit, int_of_string second_digit)
;;

let parse_instructions =
  many (parse_instruction >>| (fun pair -> Some pair) <|> any_char *> return None)
  >>| List.filter_map ~f:(fun x ->
    match x with
    | None -> None
    | Some pair -> Some pair)
;;

let run_instructions pairs =
  List.fold pairs ~init:0 ~f:(fun sum (first, second) ->
    let product = first * second in
    sum + product)
;;

let () =
  let day = DayThree Puzzle in
  let puzzle = Utils.get_puzzle day in
  let result = parse_string ~consume:All parse_instructions puzzle in
  match result with
  | Ok pairs -> run_instructions pairs |> string_of_int |> print_endline
  | Error msg -> failwith msg
;;
