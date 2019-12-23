let rec parse_lines acc ch =
  match Scanf.bscanf ch "%d\n" (fun x -> x) with
  | exception _ -> acc
  | x -> parse_lines (x :: acc) ch

let lines =
  let input = Scanf.Scanning.open_in "input.txt" in
  let lines = parse_lines [] input in
  Scanf.Scanning.close_in input;
  List.rev lines

module S = Set.Make (struct
  type t = int

  let compare = compare
end)

let rec search_dup known cur = function
  | [] -> search_dup known cur lines
  | x :: rest ->
    let cur = cur + x in
    let known' = S.add cur known in
    if known == known' then cur else search_dup known' cur rest

let res = search_dup (S.singleton 0) 0 []

let () = Printf.printf "%d\n" res
