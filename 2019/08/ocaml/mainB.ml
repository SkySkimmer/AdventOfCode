
let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let width = 25
let height = 6

let size = width * height

(* 3: invalid color, if it remains at the end we missed some pixel *)
let layer = Array.make size 3

let rec modnth f l n = match l, n with
  | [], _ -> abort "setnth: bad index"
  | x :: tl, 0 -> f x :: tl
  | x :: tl, _ -> x :: modnth f tl (n-1)

let set_one i v =
  let i = i mod size in
  if layer.(i) = 3 (* unset *) then layer.(i) <- v

let read_one ch i =
  let c = input_char ch in
  if c = '\n' then raise End_of_file;
  match c with
    | '0' -> set_one i 0
    | '1' -> set_one i 1
    | '2' -> ()
    | _ -> abort "unexpected color %c (index %d)\n" c i

let rec read_all ch i =
  match read_one ch i with
  | exception End_of_file -> debug "count of %d (%d)\n" i (i mod size)
  | () ->
    read_all ch (i+1)

let () =
  let input = open_in "input.txt" in
  let () = read_all input 0 in
  close_in input

let () = Array.iteri (fun i p ->
    let () = match p with
      (* NB inverted colors are more readable *)
      | 0 -> print_string " "
      | 1 -> print_string "█"
      | _ -> abort "unexpected color %d\n" p
    in
    if i mod width = width - 1 then print_char '\n')
    layer
