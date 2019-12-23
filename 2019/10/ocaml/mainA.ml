let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

type vec = { x : int; y : int }

let rec load_astra pos nastra astra ch =
  match input_char ch with
  | exception End_of_file -> (astra, nastra)
  | '\n' -> load_astra { x = 0; y = pos.y + 1 } nastra astra ch
  | '.' -> load_astra { x = pos.x + 1; y = pos.y } nastra astra ch
  | '#' ->
    load_astra { x = pos.x + 1; y = pos.y } (nastra + 1) (pos :: astra) ch
  | c -> abort "unknown symbol %c\n" c

let astra, nastra =
  let input = open_in "input.txt" in
  let r = load_astra { x = 0; y = 0 } 0 [] input in
  close_in input;
  r

let () = debug "%d asteroids\n" nastra

let vdiff a b = { x = a.x - b.x; y = a.y - b.y }

let sign n = compare n 0

let same_direction a b =
  sign a.x = sign b.x && sign a.y = sign b.y && a.y * b.x = b.y * a.x

let mk x y = { x; y }

let () =
  debug "true=%b false=%b false=%b false=%b\n"
    (same_direction (mk 0 1) (mk 0 2))
    (same_direction (mk 0 (-1)) (mk 0 2))
    (same_direction (mk 0 1) (mk 1 2))
    (same_direction (mk 1 2) (mk 2 3))

let is_hid base seen other =
  List.exists
    (fun seen -> same_direction (vdiff seen base) (vdiff other base))
    seen

let rec best_count_hidden best hid seen base = function
  | [] -> hid
  | other :: rest ->
    if is_hid base seen other then
      if hid + 1 = best then best
      else best_count_hidden best (hid + 1) seen base rest
    else best_count_hidden best hid (other :: seen) base rest

let rec find_best best = function
  | [] -> best
  | v :: rest ->
    let best = best_count_hidden best 0 [] v astra in
    find_best best rest

let best = find_best nastra astra

(* we count the number of astra hidden from the best point. -1 because
   we don't count the station. *)
let () = Printf.printf "%d\n" (nastra - best - 1)
