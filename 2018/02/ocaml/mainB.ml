let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let rec read_all ch lines =
  match input_line ch with
  | exception End_of_file -> lines
  | l -> read_all ch (l :: lines)

let lines =
  let input = open_in "input.txt" in
  let r = read_all input [] in
  close_in input;
  r

let rec eq_from_to a b i j =
  if i = j then true
  else if a.[i] <> b.[i] then false
  else eq_from_to a b (i + 1) j

let without s len i =
  if i + 1 = len then String.sub s 0 i
  else String.sub s 0 i ^ String.sub s (i + 1) (len - i - 1)

let check_oneoff a b =
  let len = String.length a in
  if len <> String.length b then None
  else
    let rec aux i =
      if i = len then None
      else if a.[i] <> b.[i] then
        if eq_from_to a b (i + 1) len then Some (without a len i) else None
      else aux (i + 1)
    in
    aux 0

let rec find_oneoff base = function
  | [] -> None
  | x :: rest ->
    (match check_oneoff base x with
    | Some _ as r -> r
    | None -> find_oneoff base rest)

let rec find = function
  | [] -> abort "nothing found"
  | x :: rest ->
    (match find_oneoff x rest with Some r -> r | None -> find rest)

let res = find lines

let () = Printf.printf "%s\n" res
