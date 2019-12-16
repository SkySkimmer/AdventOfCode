
let debugging = match Sys.argv with
  | [|_;"-debug"|] -> true
  | _ -> false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let l0 =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  l

let l0 = Array.init (String.length l0) (fun i -> int_of_string (String.make 1 (String.get l0 i)))

(* produce nth digit for the list after l *)
let pass1 l i =
  let rec aux step v =
    if step = Array.length l then (abs v) mod 10
    else
      (* step+1: shift pattern left by 1
         i+1: repeats in the pattern *)
      let v = match ((step+1) / (i+1)) mod 4 with
        | 0 | 2 -> v
        | 1 -> v + Array.get l step
        | 3 -> v - Array.get l step
        | _ -> assert false
      in
      aux (step+1) v
  in
  aux 0 0

let pass l =
  Array.init (Array.length l) (pass1 l)

let rec passes l i =
  if i = 0 then l
  else passes (pass l) (i-1)

let l = passes l0 100

let () = Array.iter print_int (Array.sub l 0 8); print_newline()
