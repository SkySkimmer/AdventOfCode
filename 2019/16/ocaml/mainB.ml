
let debugging = match Sys.argv with
  | [|_;"-debug"|] -> true
  | _ -> false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let l0 =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  l

let offset = int_of_string (String.sub l0 0 7)
let l0 = Array.init (String.length l0) (fun i -> int_of_string (String.make 1 (String.get l0 i)))

(* input length is 650 -> repeat 10k: len=6.5M
   first 7 digits are idx=5979673 (a bit less than 6M)
   we need to do 100 passes then get 8 digits starting at idx

   Suppose input is 12345678

   for digit 5 (> 8/2) pattern is
   00001111...
       ^ <- idx 5
   for digit 6
   00000111...
        ^ <- idx 6

   so at i > len/2, pass(orig)[i] = sum(j=i..len-1)orig[j] mod 10

   with v(t) the vector of points starting len/2+1 at time t,
   v(t+1) = M * v(t)
   where M is a square matrix with M(i,j) = (i >= j) ? 1 : 0 (i col, j line)
   ie M(i+k,i) = 1
   thus v(100) = M^100 * v(0)

   in general M^k(i+n,i) = binom(n+k,k) = binom(n+k,n) = (n+k)! / (n! * k!)
   (reddit said it was binom, this is the binom equation that fits experiments for small M and k)

   v(n)[i] = (M^n * v)[i] = sum(k=0..end)(M^n[k,i] * v[k])
           = sum(k=0..end-i)(M^n[i+k,i] * v[k])
           = sum(k=0..end-i)(binom(n+k,n) * v[k])
   what is end? 6.5M? I guess we can compute 500k*8 of the things(?)

   https://gist.github.com/alexanderhaupt/1ac31ecbd316aca32c469f42d8646c98 via reddit
   says chinese remainder + lukas theorem for fast mod10

   let's be honest at this point I'm just translating python to ocaml
*)

let rec binom n k =
  if k = 0 then 1
  else n * binom (n-1) (k-1) / k

let rec biprime n k p =
  if k = 0 then 1
  else if n < p && k < p then (binom n k) mod p
  else (biprime (n / p) (k / p) p * biprime (n mod p) (k mod p) p) mod p

let bimod10 n k =
  ((biprime n k 2) * 5 + (biprime n k 5) * 6) mod 10

let len = Array.length l0
let fulllen = len * 10_000

let repeat = 100

let rec sum f a b acc =
  if a = b then acc
  else sum f (a+1) b ((acc + f a) mod 10)

let digits = Array.init 8 (fun i ->
    let i = offset + i in
    sum (fun j -> l0.((i+j) mod len) * bimod10 (repeat - 1 + j) j) 0 (fulllen - i) 0)

let () = Array.iter print_int digits; print_newline()
