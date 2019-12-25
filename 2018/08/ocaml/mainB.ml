open Utils.Util

let l =
  let input = open_in "../input.txt" in
  let l = input_line input in
  close_in input;
  l

let l = List.map int_of_string (String.split_on_char ' ' l)

let () = debug 1 "%d entries\n" (List.length l)

let value_of meta = function
  | [] -> List.fold_left ( + ) 0 meta
  | children ->
    let children = Array.of_list children in
    let nnodes = Array.length children in
    List.fold_left
      (fun sum meta ->
        if meta <= nnodes then sum + children.(meta - 1) else sum)
      0 meta

let rec parse_one = function
  | nnodes :: nmeta :: rest ->
    debug 1 "nnodes %d nmetas %d\n" nnodes nmeta;
    let children, rest = parse_nodes nnodes [] rest in
    let meta, rest = parse_meta nmeta [] rest in
    let v = value_of meta children in
    debug 1 "v=%d\n" v;
    (v, rest)
  | [ _ ] | [] -> abort "nothing to parse1\n"

and parse_nodes nnodes acc data =
  if nnodes = 0 then (List.rev acc, data)
  else
    let next, data = parse_one data in
    parse_nodes (nnodes - 1) (next :: acc) data

and parse_meta nmeta acc data =
  if nmeta = 0 then (List.rev acc, data)
  else
    match data with
    | [] -> abort "nothing to parse_meta\n"
    | next :: data -> parse_meta (nmeta - 1) (next :: acc) data

let r, rest = parse_one l

let () = abort_unless (rest = []) "didn't parse everything!\n"

let () = Printf.printf "%d\n" r
