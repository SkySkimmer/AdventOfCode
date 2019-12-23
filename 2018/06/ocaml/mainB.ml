open Utils.Util

let lines =
  let input = open_in "../input.txt" in
  let l = input_lines input in
  close_in input;
  l

let coords =
  List.map
    (fun line ->
      match String.split_on_char ',' line with
      | [ l; r ] ->
        (int_of_string (String.trim l), int_of_string (String.trim r))
      | _ -> abort "could not parse %S\n" line)
    lines

let maxx, maxy =
  List.fold_left
    (fun (maxx, maxxy) (x, y) -> (max maxx x, max maxxy y))
    (0, 0) coords

let dist (x, y) (x', y') = abs (x - x') + abs (y - y')

let cost x = List.fold_left (fun cost coord -> cost + dist x coord) 0 coords

let area = ref 0

let () =
  for i = 0 to maxx do
    for j = 0 to maxy do
      if cost (i, j) < 10000 then incr area
    done
  done

let area = !area

let () = Printf.printf "%d\n" area
