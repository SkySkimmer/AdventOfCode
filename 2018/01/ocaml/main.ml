
let rec parse_lines acc ch =
  match Scanf.bscanf ch "%d\n" (fun x -> x) with
  | exception _ -> acc
  | x -> parse_lines (acc+x) ch

let res =
  let input = Scanf.Scanning.open_in "input.txt" in
  let res = parse_lines 0 input in
  Scanf.Scanning.close_in input;
  res

let () = Printf.printf "%d\n" res
