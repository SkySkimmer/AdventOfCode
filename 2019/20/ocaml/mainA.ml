let debugging =
  match Sys.argv with
  | [| _; "-debug" |] -> 1
  | [| _; "-debug"; s |] -> int_of_string s
  | _ -> 0

let debug lvl m =
  if debugging >= lvl then Format.eprintf m
  else Format.ifprintf Format.err_formatter m

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
(* Empty is the only walkable *)
type tile = Wall | Empty | Letter of char | Void

let stile = function
  | Wall -> "#"
  | Empty -> "."
  | Letter c -> String.make 1 c
  | Void -> " "

let prtile ch t = Printf.fprintf ch "%s" (stile t)

let parse_tile = function
  | '#' -> Wall
  | '.' -> Empty
  | 'A' .. 'Z' as d -> Letter d
  | ' ' -> Void
  | c -> abort "unknown tile %c\n" c

let parse_line len l =
  Array.init len (fun j ->
      if j < String.length l then parse_tile l.[j] else Void)

let rec get_lines len ch acc =
  match input_line ch with
  | exception End_of_file -> List.rev acc
  | l ->
    let acc = parse_line len l :: acc in
    get_lines len ch acc

(* saving may have removed end of line padding whitespace, such that
   lines have different length. The first 2 lines only have some
   letters so are shorter.
   The 3rd line is (maxlen-2) if it ends with a wall, otherwise maxlen. *)
let get_lines ch =
  let l1 = input_line ch in
  let l2 = input_line ch in
  let l3 = input_line ch in
  let len3 = String.length l3 in
  let maxlen = if l3.[len3 - 1] = '#' then len3 + 2 else len3 in
  get_lines maxlen ch (List.rev_map (parse_line maxlen) [ l1; l2; l3 ])

let tiles =
  let input = open_in "input.txt" in
  let tiles = get_lines input in
  close_in input;
  tiles

let linelen = Array.length (List.hd tiles)

let nlines = List.length tiles

let () = debug 1 "linelen=%d,linec=%d\n" linelen (List.length tiles)

let () =
  abort_unless
    (List.for_all (fun l -> Array.length l = linelen) tiles)
    "inconsistent line length"

let tiles = Array.concat tiles

let idx (i, j) = (i * linelen) + j

let unidx k = (k / linelen, k mod linelen)

let get i = if 0 <= i && i < Array.length tiles then tiles.(i) else Void

let dirs =
  List.map idx
    [
      (-1, 0) (* up *);
      (1, 0) (* down *);
      (0, -1) (* left *);
      (0, 1);
      (* right *)
    ]

module Portal = String
module PortMap = Map.Make (Portal)

(* revmap is a map portal |-> locations connected to the portal *)
let rec find_portals revmap k =
  if k = Array.length tiles then revmap
  else
    let revmap =
      match tiles.(k) with
      | Wall | Empty | Void -> revmap
      | Letter a ->
        (* if a is next to an Empty tile, the other direction is the
           other half of the portal name *)
        dirs
        |> (revmap
           |> List.fold_left (fun revmap dir ->
                  if get (k + dir) <> Empty then revmap
                  else
                    match get (k - dir) with
                    | Letter b ->
                      let a, b = if dir < 0 then (a, b) else (b, a) in
                      let s = String.of_seq (List.to_seq [ a; b ]) in
                      debug 1 "found %s\n" s;
                      PortMap.update s
                        (function
                          | None -> Some [ k + dir ]
                          | Some l -> Some ((k + dir) :: l))
                        revmap
                    | _ ->
                      let i, j = unidx k in
                      abort "missing letter at %d,%d?\n" i j))
    in
    find_portals revmap (k + 1)

let portals = find_portals PortMap.empty 0

module Point = struct
  type t = int

  let compare : t -> t -> int = compare
end

module PMap = Map.Make (Point)
module PSet = Set.Make (Point)

let portals, start, goal =
  let start = ref 0 in
  let goal = ref 0 in
  let portals =
    PortMap.fold
      (fun pname points portals ->
        let bad () = abort "found not 2 portals for %s\n" pname in
        match points with
        | [ a; b ] ->
          let portals = PMap.add a b portals in
          PMap.add b a portals
        | [ a ] ->
          if pname = "AA" then start := a
          else if pname = "ZZ" then goal := a
          else bad ();
          portals
        | _ -> bad ())
      portals PMap.empty
  in
  abort_unless (!start <> 0) "not found start\n";
  abort_unless (!goal <> 0) "not found goal\n";
  (portals, !start, !goal)

let neighbours p =
  let n = List.map (( + ) p) dirs in
  let n = List.filter (fun p -> get p = Empty) n in
  match PMap.find_opt p portals with None -> n | Some p' -> p' :: n

let search () =
  let work = Queue.create () in
  let () = Queue.add (start, 0) work in

  let rec aux visited =
    match Queue.take work with
    | exception Queue.Empty -> abort "not reached goal\n"
    | p, cost ->
      let v' = PSet.add p visited in
      if v' == visited then aux visited
      else if p = goal then cost
      else begin
        List.iter (fun p -> Queue.add (p, cost + 1) work) (neighbours p);
        aux v'
      end
  in
  aux PSet.empty

let cost = search ()

let () = Printf.printf "%d\n" cost
