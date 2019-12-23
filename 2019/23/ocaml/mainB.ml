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

let () = if debugging >= 3 then Intcode.debug := true

let start_state =
  let input = open_in "input.txt" in
  let r = Intcode.input_program input in
  close_in input;
  r

type fstate = {
  state : Intcode.state;
  address : int;
  mutable waits : int;
  input : (int * int) Queue.t;
  mutable output_buf : (int * int option) option;
      (* the output buffer contains at most 2 ints *)
}

let run = Intcode.run

let run1 state =
  match run state.state with
  | Done -> abort "unexpected halt\n"
  | WaitInput k ->
    (match Queue.take state.input with
    | exception Queue.Empty ->
      k (-1);
      state.waits <- state.waits + 1;
      None
    | x, y ->
      k x;
      state.waits <- 0;
      (match run state.state with
      | Done | HaveOutput _ -> abort "expected another input instr\n"
      | WaitInput k ->
        k y;
        None))
  | HaveOutput o ->
    state.waits <- 0;
    (match state.output_buf with
    | None ->
      state.output_buf <- Some (o, None);
      None
    | Some (a, None) ->
      state.output_buf <- Some (a, Some o);
      None
    | Some (a, Some x) ->
      state.output_buf <- None;
      Some (a, (x, o)))

let boot_state i =
  let state = Intcode.make_state ~memsize:10_000 start_state in
  match run state with
  | Done | HaveOutput _ -> abort "bad boot\n"
  | WaitInput k ->
    k i;
    state

let states =
  Array.init 50 (fun i ->
      {
        state = boot_state i;
        address = i;
        waits = 0;
        input = Queue.create ();
        output_buf = None;
      })

exception Finished of int

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

(* idles = successive idles *)
let rec run_all ~idles ~nat ~lastnat cur =
  let next = (cur + 1) mod 50 in
  match run1 states.(cur) with
  | None ->
    let idles = if states.(cur).waits >= 2 then idles + 1 else 0 in
    if idles < 50 then run_all ~idles ~nat ~lastnat next
    else if lastnat = snd nat then lastnat
    else begin
      Queue.add nat states.(0).input;
      run_all ~idles:0 ~nat ~lastnat:(snd nat) 0
    end
  | Some (a, packet) ->
    if a = 255 then run_all ~idles:0 ~nat:packet ~lastnat next
    else begin
      Queue.add packet states.(a).input;
      run_all ~idles:0 ~nat ~lastnat next
    end

let r = run_all ~idles:0 ~nat:(0, 0) ~lastnat:0 0

let () = Printf.printf "%d\n" r
