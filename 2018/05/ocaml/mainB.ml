let l =
  let input = open_in "../input.txt" in
  let l = input_line input in
  close_in input;
  l

let l = List.of_seq (String.to_seq l)

let react a b = a <> b && Char.lowercase_ascii a = Char.lowercase_ascii b

let rec consume stack = function
  | [] -> List.length stack
  | a :: b :: rest when react a b -> consume stack rest
  | a :: rest ->
    (match stack with
    | b :: stack when react a b -> consume stack rest
    | _ -> consume (a :: stack) rest)

let with_remove1 =
  List.init 26 (fun i ->
      let ci = char_of_int (int_of_char 'a' + i) in
      let cI = char_of_int (int_of_char 'A' + i) in
      List.filter (fun c -> c <> ci && c <> cI) l)

let rs = List.map (consume []) with_remove1

let r = List.fold_left min max_int rs

let () = Printf.printf "%d\n" r
