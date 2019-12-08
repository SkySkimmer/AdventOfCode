
let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let width = 25
let height = 6

let size = width * height

(* we only do individual counts for digits 0,1 and 2 so the array has
   length 3 *)
type layer = { count : int; counts : int list }
let init_layer = { count = 0; counts = [0;0;0] }

let rec modnth f l n = match l, n with
  | [], _ -> abort "setnth: bad index"
  | x :: tl, 0 -> f x :: tl
  | x :: tl, _ -> x :: modnth f tl (n-1)

let read_one ch acc =
  let c = input_char ch in
  if c = '\n' then raise End_of_file;
  let incr i = { acc with counts = modnth succ acc.counts i } in
  let acc = match c with
    | '0' -> incr 0
    | '1' -> incr 1
    | '2' -> incr 2
    | _ -> acc
  in
  { acc with count = acc.count + 1 }

let rec read_layer ch acc =
  if acc.count = size then acc
  else
    let acc = read_one ch acc in
    read_layer ch acc

type result = { count0 : int; checksum : int }

let check_layer layer =
  abort_unless (layer.count = size) "incomplete layer";
  match layer.counts with
  | [c0;c1;c2] -> { count0 = c0; checksum = c1 * c2 }
  | _ -> abort "invalid layer data"

let max_result r1 r2 =
  if r1.count0 <= r2.count0 then r1 else r2

let rec read_all ch best =
  match read_one ch init_layer with
  | exception End_of_file -> best
  | layer ->
    let layer = read_layer ch layer in
    let best = max_result best (check_layer layer) in
    read_all ch best

let read_all ch = read_all ch (check_layer (read_layer ch init_layer))

let res =
  let input = open_in "input.txt" in
  let r = read_all input in
  close_in input;
  r

let () = Printf.printf "%d\n" res.checksum
