let bot, top =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  Scanf.sscanf l "%d-%d" (fun a b -> (a, b))

let rec good ~prev ~dubs n i =
  if i = String.length n then dubs
  else
    let cur = n.[i] in
    if cur < prev then false
    else good ~prev:cur ~dubs:(dubs || cur = prev) n (i + 1)

let good n =
  let n = string_of_int n in
  assert (String.length n = 6);
  good ~prev:'0' ~dubs:false n 0

let rec search ~found ~bot ~top =
  let found = if good bot then found + 1 else found in
  if bot = top then found else search ~found ~bot:(bot + 1) ~top

let () = Printf.printf "%d" (search ~found:0 ~bot ~top)
