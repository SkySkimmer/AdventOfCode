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

type action = ReDeal | Cut of int | Deal of int

let parse_action l =
  if l = "deal into new stack" then ReDeal
  else
    match String.split_on_char ' ' l with
    | [ "cut"; n ] -> Cut (int_of_string n)
    | [ "deal"; "with"; "increment"; n ] -> Deal (int_of_string n)
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
  List.rev l

let big = Big_int.big_int_of_int

let small = Big_int.int_of_big_int

let len = 119315717514047

let blen = big len

let even n = n mod 2 = 0

let ( * ) = Big_int.mult_big_int

let ( + ) = Big_int.add_big_int

let ( mod ) a b =
  let m = Big_int.mod_big_int a b in
  if Big_int.lt_big_int m Big_int.zero_big_int then m + b else m

let ( / ) = Big_int.div_big_int

let ( - ) = Big_int.sub_big_int

let z = Big_int.zero_big_int

let u = Big_int.unit_big_int

(* [egcd a b] returns [g,x,y] such that [a*x + b*y = g] and [g] is the
   gcd of [a] and [b]. *)
let rec egcd a b =
  if Big_int.eq_big_int a z then (b, z, u)
  else
    let g, x, y = egcd (b mod a) a in
    let y' = y - (b / a * x) in
    (g, y', x)

(* [reverse ~len a] is such that [act ~len (act ~len i a) (reverse ~len a) = i] *)
let reverse ~len = function
  | ReDeal -> ReDeal
  | Cut n -> Cut (-n)
  | Deal n ->
    let g, x, _y = egcd (big n) blen in
    abort_unless
      (Big_int.eq_big_int g u && Big_int.le_big_int x blen)
      "bad deal %d for len %d\n" n len;
    (* n*x + len*_y = 1, ie n*x = 1 mod len *)
    Deal (small x)

(* [act a] produces [x,y] such that index [i] goes to [i * x + y] (mod len) when applying [a]. *)
let act = function
  | ReDeal -> (-1, -1) (* len - i - 1 = -i + len - 1 = -i - 1 mod len *)
  | Cut n -> (1, -n)
  | Deal n -> (n, 0)

(*
i1 = i0 * a1 + b1
i2 = i1 * a2 + b2

i2 = (i0 * a1 + b1) * a2 + b2
   = i0 * a1 * a2 + b1 * a2 + b2
a2' = a1 * a2
b2' = b1 * a2 + b2
*)
let combine (a1, b1) (a2, b2) = (a1 * a2 mod blen, ((b1 * a2) + b2) mod blen)

let a, b =
  List.fold_left
    (fun x1 a ->
      let a, b = act a in
      combine x1 (big a, big b))
    (big 1, big 0)
    (* careful! we need to apply the reversed actions in reverse *)
    (List.rev_map (reverse ~len) actions)

(* a,b reverses one round of shuffling *)

(*
   twice is a^2, (a+1)*b
   three is a^3, (a^2 + a + 1)*b
   four times is a^4, (a^3 + a^2 + a + 1)*b
   five times is a^5, (a^4 + a^3 + a^2 + a + 1)*b

   n times is a^n, (a^(n-1) + a^(n-2) + ... + a + 1)*b
   let sn = a^(n-1) + a^(n-2) + ... + a + 1

   (a^3 + a^2 + a + 1) = (a + 1)*a^2 + a + 1 = (a + 1) (a^2 + 1)
   s4 = s2 * (a^2 + 1)


   (a^5 + ... + 1) = (a^2 + a + 1) * a^3 + a^2 + a + 1
   s6 = s3 * (a^3 + 1)

   s(2n) = sn * (a^n + 1)
   a^(2n) = (a^n)^2

   s(2n+1) = a^(2n) + s(2n)
   a^(2n+1) = a^(2n) * a

   [coeffs n] returns [a^n, sn]
*)
let rec coeffs n =
  if n = 0 then (big 1, big 0)
  else if n = 1 then (a, big 1)
  else if even n then
    let ak, sk = coeffs Stdlib.(n / 2) in
    (ak * ak mod blen, sk * (ak + Big_int.unit_big_int) mod blen)
  else
    (* odd n *)
    let ak, sk = coeffs Stdlib.(n - 1) in
    (ak * a mod blen, (ak + sk) mod blen)

let an, sn = coeffs 101741582076661

let r = small (((an * big 2020) + (sn * b)) mod blen)

let () = Printf.printf "%d\n" r
