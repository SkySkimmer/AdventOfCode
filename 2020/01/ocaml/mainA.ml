open Utils.Util

let l =
  let input = open_in "../input.txt" in
  let l = List.map int_of_string (input_lines input) in
  close_in input;
  l

let () = debug 1 "%d entries\n" (List.length l)

let l = Array.of_list l

let () = Array.sort compare l

let target = 2020

let () =
  Array.iter
    (fun v1 ->
      Array.iter
        (fun v2 ->
          if v1 + v2 = target then Printf.printf "found %d\n" (v1 * v2))
        l)
    l
