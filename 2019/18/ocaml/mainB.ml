let debugging = match Sys.argv with
  | [|_;"-debug"|] -> 1
  | [|_;"-debug";s|] -> int_of_string s
  | _ -> 0

let debug lvl m = if debugging >= lvl then Format.eprintf m else Format.ifprintf Format.err_formatter m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let rec prlist pr sep ch = function
  | [] -> ()
  | [x] -> pr ch x
  | x :: rest -> pr ch x; Format.fprintf ch sep; prlist pr sep ch rest

(* convention: points are (i,j) where i is line number and j col number *)
type point = int * int

type content = Key of int | Door of int

type tile = Wall | Empty of content option

let nkeys = int_of_char 'z' - int_of_char 'a' + 1

let keys = Array.make nkeys (0,0)
let doors = Array.make nkeys (0,0)

let key_idx k = int_of_char k - int_of_char 'a'
let door_idx d = int_of_char d - int_of_char 'A'

let pkey k = char_of_int (k + int_of_char 'a')
let pdoor d = char_of_int (d + int_of_char 'A')

let start = ref (0,0)

let parse_tile pos = function
  | '#' -> Wall
  | '.' -> Empty None
  | '@' -> start := pos; Empty None
  | 'a'..'z' as k -> let k = key_idx k in keys.(k) <- pos; Empty (Some (Key k))
  | 'A'..'Z' as d -> let d = door_idx d in doors.(d) <- pos; Empty (Some (Door d))
  | c -> abort "unknown tile %c\n" c

let parse_line i l =
  Array.init (String.length l) (fun j -> parse_tile (i,j) (String.get l j))

let rec get_lines ch i acc =
  match input_line ch with
  | exception End_of_file -> List.rev acc
  | l ->
    let acc = parse_line i l :: acc in
    get_lines ch (i+1) acc

let tiles =
  let input = open_in "input.txt" in
  let tiles = get_lines input 0 [] in
  close_in input;
  tiles

let start = !start

let linelen = Array.length (List.hd tiles)

let () = debug 1 "linelen=%d,linec=%d\n" linelen (List.length tiles)

let () = abort_unless (List.for_all (fun l -> Array.length l = linelen) tiles)
    "inconsistent line length"

let () = Array.iteri (fun k p -> abort_unless (p <> (0,0)) "missing key %c\n" (pkey k)) keys

let () = abort_unless (start <> (0,0)) "didn't find start\n"

let tiles = Array.concat tiles

let idx (i,j) = i*linelen + j
let get p = tiles.(idx p)

let (++) (x,y) (x',y') = (x+x',y+y')

let dirs = [
  (-1,0) (* up *);
  (1,0) (* down *);
  (0,-1) (* left *);
  (0,1) (* right *);
]

let neighbours p = List.map ((++) p) dirs

let () = (neighbours start) |> List.iter (fun p ->
    tiles.(idx p) <- Wall)
let starts = List.map ((++) start) [(1,1);(-1,1);(-1,-1);(1,-1)]
(* SE, NE, NW, SW *)

let getbit i b = (i lsr b) mod 2 <> 0
let setbit i b = if getbit i b then i else i + (1 lsl b)
let imply_bits a b = (a land b) = a

module I = struct type t = int let compare = compare end
module IntMap = Map.Make(I)
module IntSet = Set.Make(I)

(* paths are unique: we keep the list of encountered doors next to the cost
   (thanks reddit, not sure why they were talking about dfs though)  *)
let dists_from start =
  let work = Queue.create () in
  let () = Queue.add (start,0,0) work in
  let dists = ref IntMap.empty in
  let visited = Array.make (Array.length tiles) false in
  let rec aux () =
    match Queue.take work with
    | exception Queue.Empty -> !dists
    | (p,cost,doors) -> if visited.(idx p) then aux ()
      else begin
        visited.(idx p) <- true;

        if p <> start then
          (match get p with
           | Empty (Some (Key k)) ->
             dists := !dists |> IntMap.update k (function
                 | None -> Some (cost,doors)
                 | Some _ -> abort "bad\n" (* ensure uniqueness *))
           | _ -> ());

        List.iter
          (fun p -> match get p with
             | Wall -> ()
             | Empty (Some (Door d)) -> Queue.add (p,cost+1,setbit doors d) work
             | Empty _ -> Queue.add (p,cost+1,doors) work)
          (neighbours p);
        aux ()
      end
  in
  aux ()

(* put the start points at the end *)
let dists = Array.init (nkeys+4) (fun i ->
    IntMap.bindings
      (if i < nkeys then dists_from keys.(i)
       else dists_from (List.nth starts (i - nkeys))))

let allkeys = (1 lsl nkeys) - 1

let (>>=) a f =
  List.concat (List.rev_map f a)

let replace_nth l i v = Array.mapi (fun j vj -> if i = j then v else vj) l

let possible_moves bots keys =
  List.init 4 (fun i -> i) >>= fun i ->
  let bot = bots.(i) in
  dists.(bot) |> List.filter (fun (target,(_,doors)) ->
      not (getbit keys target) && imply_bits doors keys)
  |> List.map (fun (target,(cost,_)) ->
      let bots = replace_nth bots i target in
      bots, setbit keys target, cost)

let ppint ch i = Format.fprintf ch "%d" i

module PSet = Set.Make(struct type t = int array * int let compare = compare end)

type pview = Empty | Node of {l:pview; v:PSet.elt; r:pview; h:int}

(* mem p set iff there is an element of set which has the same bots and has better keys
   NB: better keys are always >= as ints *)
let rec mem (bots,keys as x) = function
  | Empty -> false
  | Node {l;v=(bots',keys');r;_} ->
    let c = compare bots bots' in
    if c < 0 then mem x l
    else if c > 0 then mem x r
    else if imply_bits keys keys' then true
    else let c = compare keys keys' in
      if c < 0 then mem x l
      else mem x r

let mem x (set:PSet.t) = mem x (Obj.magic set)

let insert bots keys points = (bots,keys) :: (List.filter (fun (bots',keys') ->
    not (bots = bots' && imply_bits keys' keys))
    points)

let cost =
  let work = ref (IntMap.singleton 0 [Array.init 4 (fun i -> i + nkeys),0]) in
  let visited = ref PSet.empty in
  let exception Found of int in
  let rec aux () =
    match IntMap.min_binding_opt !work with
    | None -> abort "not found\n"
    | Some (cost,points) ->
      work := IntMap.remove cost !work;
      points |> List.iter (fun (bots,keys as p) ->
          if mem p !visited then ()
          else begin
            visited := PSet.add p !visited;
            if keys = allkeys then raise (Found cost);
            possible_moves bots keys |> List.iter (fun (bots,keys,mcost) ->
                let cost = cost + mcost in
                work := !work |> IntMap.update cost (function
                    | None -> Some [bots,keys]
                    | Some points -> Some (insert bots keys points)))
          end);
      aux ()
  in
  try aux () with Found i -> i

let () = Printf.printf "%d\n" cost
