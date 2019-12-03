
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

(* undirected segments: normalized to be always up or right *)
type dir' = Vertical | Horizontal

let make_segment (x,y) (d,n) = match d with
  | Up -> Vertical, n, (x,y)
  | Down -> Vertical, n, (x,y-n)
  | Left -> Horizontal, n, (x-n, y)
  | Right -> Horizontal, n, (x,y)

let rec make_segments start acc = function
  | [] -> List.rev acc
  | m :: rest ->
    make_segments (move start m)
      ((make_segment start m)::acc)
      rest

let s1 = make_segments (0,0) [] m1
let s2 = make_segments (0,0) [] m2

(* returns intersection point between 2 segments *)
let intersect (d,n,(x,y)) (d',n',(x',y')) =
  if d = d' then None (* assume no parallel intersections *)
  else
    let n,n',x,y,x',y' = match d, d' with
      | Vertical, Vertical | Horizontal, Horizontal -> assert false
      | Vertical, Horizontal -> n,n',x,y,x',y'
      | Horizontal, Vertical -> n',n,x',y',x,y
    in
    (* vertical up n from x,y; horizontal right n' from x',y' *)
    if x' <= x && x <= x'+n' && y <= y' && y' <= y+n
    then Some (x,y')
    else None

let norm (x,y) = abs x + abs y

let res = List.fold_left (fun acc s1 -> List.fold_left (fun acc s2 ->
    let inter = intersect s1 s2 in
    match acc, inter with
    | None, None -> None
    | Some res, None | None, Some res -> Some res
    | Some a, Some b ->
      if norm a < norm b then Some a else Some b)
    acc s2)
    None s1

let res = match res with
  | Some res -> res
  | None -> assert false

let () = Printf.printf "%d\n" (norm res)
