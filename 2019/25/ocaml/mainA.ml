open Utils.Util

let prog = "../input.txt"

let prog =
  try
    let f = open_in prog in
    let r = Intcode.input_program f in
    close_in f;
    r
  with Sys_error e -> abort "Could not open %s: %s\n" prog e

let state = Intcode.make_state ~memsize:10_000 prog

let quiet = ref false

let wrap_print k c =
  if not !quiet then print_char c;
  k c

(* NB: output is formatted as:
- lines "== FOO ==" where FOO is the room name
- lines "- FOO" where FOO is an available direction or item
- "Command?" at the end
- lines the autoplayer doesn't care about for fluff
*)
let rec read_line buf =
  match Intcode.run_ascii state with
  | HaveOutput c ->
    if not !quiet then print_char c;
    if c = '\n' then
      if Buffer.length buf > 0 then Buffer.contents buf
      else (* skip empty lines *) read_line buf
    else begin
      Buffer.add_char buf c;
      read_line buf
    end
  | Done | WaitInput _ -> abort "expecting more output\n"

let read_line () = read_line (Buffer.create 80)

let rec feed_line ?(echo = true) l i =
  match Intcode.run_ascii state with
  | HaveOutput _ | Done -> abort "expected to give input\n"
  | WaitInput k ->
    let k = if echo then wrap_print k else k in
    if i = String.length l then k '\n'
    else begin
      k l.[i];
      feed_line ~echo l (i + 1)
    end

module SSet = Set.Make (String)
module SMap = Map.Make (String)

let rec get_command () =
  if read_line () = "Command?" then () else get_command ()

let rec find_self () =
  let l = read_line () in
  if l.[0] = '=' then begin
    get_command ();
    l
  end
  else find_self ()

let find_self () =
  (* skip first line which is the new room name *)
  ignore (read_line ());
  find_self ()

let rec get_inv inv =
  match read_line () with
  | "Command?" -> inv
  | l ->
    let inv =
      if l.[0] = '-' then String.sub l 2 (String.length l - 2) :: inv else inv
    in
    get_inv inv

let get_inv () =
  feed_line "inv" 0;
  get_inv []

let pickup item =
  feed_line ("take " ^ item) 0;
  get_command ()

let drop item =
  feed_line ("drop " ^ item) 0;
  get_command ()

(* if we see [dropped] as a big-endian integer, this is just adding 1. *)
let rec drop_next fullinv dropped i =
  if i = Array.length dropped then abort "could not find solution\n"
  else if dropped.(i) then begin
    pickup fullinv.(i);
    dropped.(i) <- false;
    drop_next fullinv dropped (i + 1)
  end
  else begin
    drop fullinv.(i);
    dropped.(i) <- true
  end

let rec tryall dir here fullinv dropped =
  drop_next fullinv dropped 0;
  feed_line dir 0;
  let here' = find_self () in
  if here = here' then tryall dir here fullinv dropped else ()

let tryall k dir =
  (* first try to go in [dir] to get the current room. *)
  let k = wrap_print k in
  k dir.[0];
  feed_line dir 1;
  (* output looks like
     == New Room ==
     Analyzing...

     [new room description]

     A loud, robotic voice says ...

     == Old Room ==
     [old room description]

     Command?
  *)
  let here = find_self () in
  let fullinv = Array.of_list (get_inv ()) in
  let dropped = Array.make (Array.length fullinv) false in
  tryall dir here fullinv dropped

let rec manual () =
  match Intcode.run_ascii state with
  | Done -> ()
  | HaveOutput o ->
    print_char o;
    manual ()
  | WaitInput k ->
    flush stdout;
    let l = input_line stdin in
    (match String.split_on_char ' ' l with
    | [ "tryall"; dir ] -> tryall k dir
    | _ ->
      if l = "" then k '\n'
      else begin
        k l.[0];
        feed_line ~echo:false l 1
      end;
      manual ())

let () = manual ()
