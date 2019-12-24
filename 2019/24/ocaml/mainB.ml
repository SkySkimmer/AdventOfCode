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

let center = idx (2, 2)

let () =
  abort_unless
    (not (getbit start center))
    "center is supposed to be empty/special\n"

(* in part B, the full state is a list of ints, where the first
   element is the outermost tile. *)

let ( ++ ) (x, y) (x', y') = (x + x', y + y')

let dirs =
  [
    (-1, 0) (* left *); (1, 0) (* right *); (0, -1) (* up *); (0, 1) (* down *);
  ]

type change = None | Inner | Outer

(* returns (layer change, position in layer) *)
let neighbours ~dir p =
  let p = unidx p in
  let nx, ny = dir ++ p in
  if nx = 2 && ny = 2 then
    (* moved to center, what direction? *)
    let dx, dy = dir in
    let ps =
      if dx < 0 then (* left *)
        List.init 5 (fun i -> idx (4, i))
      else if 0 < dx then (* right *)
        List.init 5 (fun i -> idx (0, i))
      else if dy < 0 then (* up *)
        List.init 5 (fun i -> idx (i, 4))
      else (* 0 < dy, down *)
        List.init 5 (fun i -> idx (i, 0))
    in
    List.map (fun p -> (Inner, p)) ps
  else
    (* not to center, only 1 point *)
    let n =
      if nx < 0 then (* moved left to outside *) (Outer, idx (1, 2))
      else if 5 <= nx then (* moved right to outside *) (Outer, idx (3, 2))
      else if ny < 0 then (* moved up to outside *) (Outer, idx (2, 1))
      else if 5 <= ny then (* moved down to outside *) (Outer, idx (2, 3))
      else (* normal neighbour *) (None, idx (nx, ny))
    in
    [ n ]

let neighbours =
  Array.init 25 (fun bit ->
      let ns = List.concat (List.map (fun dir -> neighbours ~dir bit) dirs) in
      List.sort_uniq compare ns)

let prpoint ch (x, y) = Format.fprintf ch "(%d,%d)" x y

let slayer = function None -> "None" | Outer -> "Outer" | Inner -> "Inner"

let prneigh ch (layer, p) =
  let p = unidx p in
  Format.fprintf ch "%s,%a" (slayer layer) prpoint p

let () =
  if debugging >= 1 then
    Array.iteri
      (fun bit ns ->
        let p = unidx bit in
        Format.printf "%a -> %a\n" prpoint p (prlist prneigh " ") ns)
      neighbours

let neighbour_count ~inner ~outer ~state bit =
  List.fold_left
    (fun count (layer, n) ->
      let has =
        match layer with
        | None -> getbit state n
        | Inner -> getbit inner n
        | Outer -> getbit outer n
      in
      if has then count + 1 else count)
    0 neighbours.(bit)

let rec step1 ~inner ~outer ~state acc bit =
  if bit = 25 then acc
  else if bit = center then step1 ~inner ~outer ~state acc (bit + 1)
  else
    let bug = getbit state bit in
    let cnt = neighbour_count ~inner ~outer ~state bit in
    let acc = if cnt = 1 || (cnt = 2 && not bug) then setbit acc bit else acc in
    step1 ~inner ~outer ~state acc (bit + 1)

let rec step_aux ~outer acc = function
  | [] -> abort "bad step\n"
  | [ state ] ->
    let state = step1 ~inner:0 ~outer ~state 0 0 in
    List.rev (state :: acc)
  | state :: (inner :: _ as rest) ->
    let state' = step1 ~inner ~outer ~state 0 0 in
    step_aux ~outer:state (state' :: acc) rest

let rec expand_aux acc = function
  | [] | [ 0 ] -> List.rev (0 :: acc)
  | x :: rest -> expand_aux (x :: acc) rest

let rec expand = function
  | 0 :: rest -> expand rest
  | states -> expand_aux [ 0 ] states

let step states = step_aux ~outer:0 [] (expand states)

let rec prstate ~state bit =
  if bit = 25 then ()
  else begin
    if bit = center then begin
      abort_unless (not (getbit state bit)) "bad center\n";
      print_char '?'
    end
    else if getbit state bit then print_char '#'
    else print_char '.';
    if bit mod 5 = 4 then print_newline ();
    prstate ~state (bit + 1)
  end

let prstates states =
  List.iter
    (fun state ->
      prstate ~state 0;
      print_newline ())
    states

let rec nsteps states n =
  if debugging >= 1 then begin
    print_endline "step";
    prstates states
  end;
  if n = 0 then states else nsteps (step states) (n - 1)

let rec count1 ~state acc bit =
  if bit = 25 then acc
  else count1 ~state (if getbit state bit then acc + 1 else acc) (bit + 1)

let count states =
  List.fold_left (fun cnt state -> count1 ~state cnt 0) 0 states

let state200 = nsteps [ start ] 200

let r = count state200

let () = Printf.printf "%d\n" r
