
type dir = Up | Down | Left | Right

let parse_dir = function
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right
  | _ -> assert false

let parse_move m =
  Scanf.sscanf m "%c%d" (fun d n -> parse_dir d, n)

let parse_line l =
  let moves = List.map parse_move (String.split_on_char ',' l) in
  moves

let m1, m2 =
  let input = open_in "input.txt" in
  let l1 = input_line input in
  let l2 = input_line input in
  close_in input;
  parse_line l1, parse_line l2

let move (x,y) (d,n) = match d with
  | Up -> x, y + n
  | Down -> x, y - n
  | Left -> x - n, y
  | Right -> x + n, y


let rec make_segments start acc = function
  | [] -> List.rev acc
  | m :: rest ->
    make_segments (move start m)
      ((start,m)::acc)
      rest

let s1 = make_segments (0,0) [] m1
let s2 = make_segments (0,0) [] m2

(* undirected segments: normalized to be always up or right *)
type dir' = Vertical | Horizontal

let make_undir ((x,y), (d,n)) = match d with
  | Up -> Vertical, n, (x,y)
  | Down -> Vertical, n, (x,y-n)
  | Left -> Horizontal, n, (x-n, y)
  | Right -> Horizontal, n, (x,y)

(* returns intersection point between 2 segments *)
let intersect_undir (d,n,(x,y)) (d',n',(x',y')) =
  if d = d' then None (* assume no parallel intersections *)
  else
    let n,n',x,y,x',y' = match d, d' with
      | Vertical, Vertical | Horizontal, Horizontal -> assert false
      | Vertical, Horizontal -> n,n',x,y,x',y'
      | Horizontal, Vertical -> n',n,x',y',x,y
    in
    (* vertical up n from x,y; horizontal right n' from x',y' *)
    if x' <= x && x <= x'+n' && y <= y' && y' <= y+n
    then if x = 0 && y' = 0 then None else Some (x,y')
    else None

let steps_to ((x,y),(d,_)) (x',y') = match d with
  | Up -> y' - y
  | Down -> y - y'
  | Right -> x' - x
  | Left -> x - x'

let best_inter acc steps1 s1 steps2 s2 =
  match intersect_undir (make_undir s1) (make_undir s2) with
  | None -> acc
  | Some inter ->
    let steps1 = steps1 + steps_to s1 inter in
    let steps2 = steps2 + steps_to s2 inter in
    let steps = steps1 + steps2 in
    match acc with
    | None -> Some steps
    | Some steps' -> if steps < steps' then Some steps else acc

let rec search_from steps1 s1 steps2 acc = function
  | [] -> acc
  | s2 :: rest ->
    let acc = best_inter acc steps1 s1 steps2 s2 in
    search_from steps1 s1 (steps2 + (snd (snd s2))) acc rest

let rec search other steps1 acc = function
  | [] -> acc
  | s1 :: rest ->
    let acc = search_from steps1 s1 0 acc other in
    search other (steps1 + (snd (snd s1))) acc rest

let res = search s2 0 None s1

let res = match res with
  | Some res -> res
  | None -> assert false

let () = Printf.printf "%d\n" res
