let debugging = match Sys.argv with
  | [|_;"-debug"|] -> 1
  | [|_;"-debug";s|] -> int_of_string s
  | _ -> 0

let debug lvl m = if debugging >= lvl then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let rec prlist pr sep ch = function
  | [] -> ()
  | [x] -> pr ch x
  | x :: rest -> pr ch x; Format.fprintf ch sep; prlist pr sep ch rest

type action =
  | ReDeal
  | Cut of int
  | Deal of int

let parse_action l =
  if l = "deal into new stack" then ReDeal
  else
    match String.split_on_char ' ' l with
    | ["cut";n] -> Cut (int_of_string n)
    | ["deal";"with";"increment";n] -> Deal (int_of_string n)
    | _ -> abort "could not understand \"%s\"\n" l

let actions =
  let input = open_in "input.txt" in
  let rec aux acc =
    match input_line input with
    | exception _ -> acc
    | l -> aux (parse_action l :: acc)
  in
  let l = aux [] in
  close_in input;
  l

(* [act ~len i a] is the index the value at [i] goes to when applying [a]. *)
let act ~len i = function
  | ReDeal -> len - i - 1
  | Cut n -> let j = i - n in (if j < 0 then j+len else j) mod len
  | Deal n -> i * n mod len

let r =
  let len = 10_007 in
  List.fold_left (act ~len) 2019 (List.rev actions)

let () = Printf.printf "%d\n" r

(** LOL misread the problem, we don't actually need to reverse... whatever *)
(* [egcd a b] returns [g,x,y] such that [a*x + b*y = g] and [g] is the
   gcd of [a] and [b]. *)
let rec egcd a b = if a = 0 then (b,0,1)
  else
    let g, x, y = egcd (b mod a) a in
    let y' = y - (b / a) * x in
    g, y', x

(* [reverse ~len a] is such that [act ~len (act ~len i a) (reverse ~len a) = i] *)
let reverse ~len = function
  | ReDeal -> ReDeal
  | Cut n -> Cut (-n)
  | Deal n ->
    let g, x, _y = egcd n len in
    abort_unless (g=1) "bad deal %d for len %d\n" n len;
    (* n*x + len*_y = 1, ie n*x = 1 mod len *)
    Deal x

let reversen ~len = List.fold_left (fun i a -> act ~len i (reverse ~len a))

let () = abort_unless (reversen ~len:10 2 [ReDeal;ReDeal;Deal 7] = 6) "bad test1\n"
