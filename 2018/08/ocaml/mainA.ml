open Utils.Util

let l =
  let input = open_in "../input.txt" in
  let l = input_line input in
  close_in input;
  l

let l = List.map int_of_string (String.split_on_char ' ' l)

let rec parse stack data acc =
  match (stack, data) with
  | [], [] -> acc
  | (0, 0) :: stack, _ -> parse stack data acc
  | (0, meta) :: stack, next :: data ->
    parse ((0, meta - 1) :: stack) data (acc + next)
  | (nodes, meta) :: stack, nodes' :: meta' :: data ->
    parse ((nodes', meta') :: (nodes - 1, meta) :: stack) data acc
  | _ :: _, ([] | [ _ ]) -> abort "insufficient data\n"
  | [], _ :: _ -> abort "too much data\n"

let r = parse [ (1, 0) ] l 0

let () = Printf.printf "%d\n" r
