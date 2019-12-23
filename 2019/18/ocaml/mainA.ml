let debugging =
  match Sys.argv with
  | [| _; "-debug" |] -> 1
  | [| _; "-debug"; s |] -> int_of_string s
  | _ -> 0

let debug lvl m =
  if debugging >= lvl then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let rec prlist pr sep ch = function
  | [] -> ()
  | [ x ] -> pr ch x
  | x :: rest ->
    pr ch x;
    Format.fprintf ch sep;
    prlist pr sep ch rest

(* convention: points are (i,j) where i is line number and j col number *)
type point = int * int

type content = Key of int | Door of int

type tile = Wall | Empty of content option

let nkeys = int_of_char 'z' - int_of_char 'a' + 1

let keys = Array.make nkeys (0, 0)

let doors = Array.make nkeys (0, 0)

let key_idx k = int_of_char k - int_of_char 'a'

let door_idx d = int_of_char d - int_of_char 'A'

let pkey k = char_of_int (k + int_of_char 'a')

let pdoor d = char_of_int (d + int_of_char 'A')

let start = ref (0, 0)

let parse_tile pos = function
  | '#' -> Wall
  | '.' -> Empty None
  | '@' ->
    start := pos;
    Empty None
  | 'a' .. 'z' as k ->
    let k = key_idx k in
    keys.(k) <- pos;
    Empty (Some (Key k))
  | 'A' .. 'Z' as d ->
    let d = door_idx d in
    doors.(d) <- pos;
    Empty (Some (Door d))
  | c -> abort "unknown tile %c\n" c

let parse_line i l =
  Array.init (String.length l) (fun j -> parse_tile (i, j) l.[j])

let rec get_lines ch i acc =
  match input_line ch with
  | exception End_of_file -> List.rev acc
  | l ->
    let acc = parse_line i l :: acc in
    get_lines ch (i + 1) acc

let tiles =
  let input = open_in "input.txt" in
  let tiles = get_lines input 0 [] in
  close_in input;
  tiles

let start = !start

let linelen = Array.length (List.hd tiles)

let () = debug 1 "linelen=%d,linec=%d\n" linelen (List.length tiles)

let () =
  abort_unless
    (List.for_all (fun l -> Array.length l = linelen) tiles)
    "inconsistent line length"

let () =
  Array.iteri
    (fun k p -> abort_unless (p <> (0, 0)) "missing key %c\n" (pkey k))
    keys

let () = abort_unless (start <> (0, 0)) "didn't find start\n"

let tiles = Array.concat tiles

let idx (i, j) = (i * linelen) + j

let get p = tiles.(idx p)

let ( ++ ) (x, y) (x', y') = (x + x', y + y')

let dirs =
  [
    (-1, 0) (* up *); (1, 0) (* down *); (0, -1) (* left *); (0, 1); (* right *)
  ]

let neighbours p = List.map (( ++ ) p) dirs

let getbit i b = (i lsr b) mod 2 <> 0

let setbit i b = if getbit i b then i else i + (1 lsl b)

(* for all i, [getbit a i] implies [getbit b i]
   ie key b is better than key a in every way *)
let imply_bits a b = a land b = a

(* compare_bits a b is -1 if we need less bits from a than from b *)
let compare_bits a b =
  if imply_bits a b then -1 else if imply_bits b a then 1 else 0

type cmp = Lt | Eq | Gt

let cmp = function -1 -> Lt | 0 -> Eq | 1 -> Gt | _ -> abort "bad cmp\n"

let walkable ~keys p =
  match get p with
  | Wall -> false
  | Empty None -> true
  | Empty (Some (Key _)) -> true
  | Empty (Some (Door d)) -> getbit keys d

type work = { point : point; keys : int; missing : int; cost : int }

let search p =
  let work = Queue.create () in
  let () = Queue.add { point = p; keys = 0; missing = nkeys; cost = 0 } work in
  let visited = Array.make (Array.length tiles) [] in
  let rec aux () =
    match Queue.take work with
    | exception Queue.Empty -> abort "couldn't finish\n"
    | w ->
      (* we have been here before with more than these keys *)
      if
        visited.(idx w.point)
        |> List.exists (fun keys' -> imply_bits w.keys keys')
      then aux ()
      else
        (* if imply_bits y keys' and imply_bits  keys' w.keys then imply_bits y w.keys
           i.e. no need to keep keys' in visited if we have w.keys *)
        let v' =
          w.keys
          :: List.filter
               (fun keys' -> not (imply_bits keys' w.keys))
               visited.(idx w.point)
        in
        visited.(idx w.point) <- v';

        let keys, missing =
          match get w.point with
          | Empty (Some (Key k)) ->
            if getbit w.keys k then (w.keys, w.missing)
            else (setbit w.keys k, w.missing - 1)
          | _ -> (w.keys, w.missing)
        in
        (* did we find the last key? *)
        if missing = 0 then w.cost
        else
          let neighbours = List.filter (walkable ~keys) (neighbours w.point) in
          List.iter
            (fun p ->
              Queue.add { point = p; keys; missing; cost = w.cost + 1 } work)
            neighbours;
          aux ()
  in
  aux ()

let cost = search start

let () = Printf.printf "%d\n" cost
