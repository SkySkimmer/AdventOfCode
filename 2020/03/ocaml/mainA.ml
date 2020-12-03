open Utils.Util
open Utils

let l =
  let input = open_in "../input.txt" in
  let l = input_lines input in
  close_in input;
  l

let l =
  l
  |> CArray.map_of_list (fun l ->
         Array.of_seq (Seq.map (( = ) '#') (String.to_seq l)))

let cnt =
  CArray.fold_left_i
    (fun i cnt l ->
      let j = i * 3 mod Array.length l in
      if l.(j) then cnt + 1 else cnt)
    0 l

let () = Printf.printf "%d\n" cnt
