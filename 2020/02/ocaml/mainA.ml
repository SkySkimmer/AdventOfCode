open Utils.Util

let l =
  let input = open_in "../input.txt" in
  let l = input_lines input in
  close_in input;
  l

let () = debug 1 "%d entries\n" (List.length l)

type entry = { least : int; most : int; letter : char; pass : string }

let parse l =
  Scanf.sscanf l "%d-%d %c: %s" (fun least most letter pass ->
      { least; most; letter; pass })

let check { least; most; letter; pass } =
  let count =
    let cnt = ref 0 in
    String.iter (fun c -> if c = letter then incr cnt) pass;
    !cnt
  in
  least <= count && count <= most

let ok_cnt =
  List.fold_left (fun cnt l -> if check (parse l) then cnt + 1 else cnt) 0 l

let () = Printf.printf "%d\n" ok_cnt
