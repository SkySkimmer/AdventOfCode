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

let slope (right, down) =
  let rec go (i, j) cnt =
    if i + down >= Array.length l then cnt
    else
      let i = i + down
      and j = (j + right) mod Array.length l.(0) in
      let cnt = if l.(i).(j) then cnt + 1 else cnt in
      go (i, j) cnt
  in
  go (0, 0) 0

let slopes = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]

let counts = List.map slope slopes

let cnt = List.fold_left ( * ) 1 counts

let () = Printf.printf "%d\n" cnt
