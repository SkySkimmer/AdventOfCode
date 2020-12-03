open Utils.Util

let l =
  let input = open_in "../input.txt" in
  let l = input_lines input in
  close_in input;
  l

let () = debug 1 "%d entries\n" (List.length l)

type entry = { pos1 : int; pos2 : int; letter : char; pass : string }

let parse l =
  Scanf.sscanf l "%d-%d %c: %s" (fun pos1 pos2 letter pass ->
      let pos1 = pos1 - 1
      and pos2 = pos2 - 1 in
      { pos1; pos2; letter; pass })

let check { pos1; pos2; letter; pass } =
  pass.[pos1] = letter != (pass.[pos2] = letter)

let ok_cnt =
  List.fold_left (fun cnt l -> if check (parse l) then cnt + 1 else cnt) 0 l

let () = Printf.printf "%d\n" ok_cnt
