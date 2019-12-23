let l =
  let input = open_in "../input.txt" in
  let l = input_line input in
  close_in input;
  l

let l = List.of_seq (String.to_seq l)

let react a b =
  abs (int_of_char a - int_of_char b) = abs (int_of_char 'A' - int_of_char 'a')

let rec consume stack = function
  | [] -> List.rev stack
  | a :: b :: rest when react a b -> consume stack rest
  | a :: rest ->
    (match stack with
    | b :: stack when react a b -> consume stack rest
    | _ -> consume (a :: stack) rest)

let r = List.length (consume [] l)

let () = Printf.printf "%d\n" r
