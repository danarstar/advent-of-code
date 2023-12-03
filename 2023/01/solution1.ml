let read_lines filename =
  let channel = open_in filename in
  let rec read_lines_helper acc =
    try
      let line = input_line channel in
      read_lines_helper (line :: acc)
    with
    | End_of_file -> close_in channel; List.rev acc
  in
  read_lines_helper []
;;

(*
  [
    "1abc2";
    "pqr3stu8vwx";
    "a1b2c3d4e5f";
    "treb7uchet"
  ]

   ["1abc2", ...] -> 11

   remove_non_digits

   ["12", "38", "1235", "7"]

   pad

   ["12", "38", "1235", "77"]

   first_and_last_only

   ["12", "38", "15", "77"]

   to_ints

   [12, 38, 15, 77]

   sum

   142

 *)

let list_of_string s = s |> String.to_seq |> List.of_seq
let string_of_char = String.make 1
let string_join = List.fold_left (^) String.empty

let is_digit c = '0' <= c && c <= '9'

let remove_non_digits = List.map
  (fun s -> list_of_string s |> List.filter is_digit |> List.map (string_of_char) |> string_join)

let pad = List.map (fun s -> if String.length s == 1 then s ^ s else s)

let get_first_char s = String.get s 0
let get_last_char s = String.get s ((String.length s) - 1)
let to_two_digits_string s = string_of_char (get_first_char s) ^ string_of_char (get_last_char s)

let first_last_digits = List.map to_two_digits_string

let to_ints = List.map int_of_string

let sum = List.fold_left (+) 0

let solution list = list
  |> remove_non_digits
  |> pad
  |> first_last_digits
  |> to_ints
  |> sum

let () = read_lines "input1.txt" |> solution |> print_int |> print_newline
