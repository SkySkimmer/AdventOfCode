open Utils.Util

(* image is 5x5=25 points, we can store state in an int *)
let getbit i b = (i lsr b) mod 2 <> 0

let setbit i b = if getbit i b then i else i + (1 lsl b)

let lines =
  let input = open_in "../input.txt" in
  let lines = input_lines input in
  close_in input;
  lines

let start, bit =
  List.fold_left
    (fun acc line ->
      Seq.fold_left
        (fun (start, bit) c ->
          let start = if c = '#' then setbit start bit else start in
          (start, bit + 1))
        acc (String.to_seq line))
    (0, 0) lines

let () = abort_unless (bit = 25) "unexpected bit count\n"

(* go between (x,y) and bit index representations *)
let idx (x, y) = x + (y * 5)

let unidx b = (b mod 5, b / 5)

let ( ++ ) (x, y) (x', y') = (x + x', y + y')

let dirs =
  [
    (-1, 0) (* left *); (1, 0) (* right *); (0, -1) (* up *); (0, 1) (* down *);
  ]

let valid (x, y) = 0 <= x && 0 <= y && x < 5 && y < 5

let neighbour_count ~state bit =
  let p = unidx bit in
  List.fold_left
    (fun count dir ->
      let p = p ++ dir in
      if valid p && getbit state (idx p) then count + 1 else count)
    0 dirs

let rec step_aux state acc bit =
  if bit = 25 then acc
  else
    let bug = getbit state bit in
    let cnt = neighbour_count ~state bit in
    let acc = if cnt = 1 || (cnt = 2 && not bug) then setbit acc bit else acc in
    step_aux state acc (bit + 1)

let step state = step_aux state 0 0

let rec search seen state =
  let state = step state in
  let seen' = IntSet.add state seen in
  if seen == seen' then state else search seen' state

let repeated = search (IntSet.singleton start) start

let () = Printf.printf "%d\n" repeated
