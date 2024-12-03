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

type instruction =
  | Multiply of (int * int)
  | Enable
  | Disable

let parse_enable_instruction = string "do()"
let parse_disable_instruction = string "don't()"

let parse_multiply_instruction =
  string "mul" *> open_paren *> digit
  >>= fun first_digit ->
  comma *> digit
  >>= fun second_digit ->
  close_paren *> return (int_of_string first_digit, int_of_string second_digit)
;;

let parse_instructions =
  many
    (parse_multiply_instruction
     >>| (fun pair -> Some (Multiply pair))
     <|> (parse_enable_instruction >>| fun _ -> Some Enable)
     <|> (parse_disable_instruction >>| fun _ -> Some Disable)
     <|> any_char *> return None)
  >>| List.filter_map ~f:(fun x ->
    match x with
    | None -> None
    | Some instruction -> Some instruction)
;;

let reducer (sum, enabled) curr =
  match enabled with
  (* This shouldn't happen since we control what `enabled` is set to. *)
  | Multiply _ -> failwith "`enabled` should never be set to `Multiply`"
  (* The current state is "disabled" *)
  | Disable ->
    (match curr with
     (* Set the state to enabled for the next iteration. *)
     | Enable -> sum, curr
     (* Otherwise, this is a no-op. *)
     | Disable | Multiply (_, _) -> sum, enabled)
  (* The current state is "enabled" *)
  | Enable ->
    (match curr with
     (* Set the state to enabled or disabled for the next iteration. *)
     | Enable | Disable -> sum, curr
     (* The current state is "enabled" and we've received a multiplication instruction, get the product of the two terms and add it to the sum . *)
     | Multiply (left, right) ->
       let product = left * right in
       sum + product, enabled)
;;

let run_instructions instructions =
  let sum, _ = List.fold instructions ~init:(0, Enable) ~f:reducer in
  sum
;;

let () =
  let day = DayThree Puzzle in
  let puzzle = Utils.get_puzzle day in
  let result = parse_string ~consume:All parse_instructions puzzle in
  match result with
  | Ok instructions -> run_instructions instructions |> string_of_int |> print_endline
  | Error msg -> failwith msg
;;
