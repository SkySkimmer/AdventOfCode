
module P = struct
  type t = int * int * int * int

  let compare : t -> t -> int = compare
end

module S = Set.Make(P)
module M = Map.Make(P)

module UF = Unionfind.Make (S) (M)

let rec parse_lines acc ch =
  match Scanf.bscanf ch "%d,%d,%d,%d\n" (fun a b c d -> a,b,c,d) with
  | exception _ -> acc
  | p -> parse_lines (p::acc) ch

let points =
  let input = Scanf.Scanning.open_in "input.txt" in
  let points = parse_lines [] input in
  Scanf.Scanning.close_in input;
  points

let close (a,b,c,d) (a',b',c',d') =
  abs (a - a') + abs (b - b') + abs (c - c') + abs (d - d') <= 3

let rec link uf p = function
  | [] -> ()
  | p' :: rest ->
    if close p p' then UF.union p p' uf;
    link uf p rest

let rec act uf = function
  | [] -> ()
  | p :: rest ->
    act uf rest;
    UF.add p uf;
    link uf p rest

let uf = UF.create ()

let () = act uf points

let classes = UF.partition uf

let () = Printf.printf "%d\n" (List.length classes)
