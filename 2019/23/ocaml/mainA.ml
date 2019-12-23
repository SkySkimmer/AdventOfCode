
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

let () = if debugging >= 3 then Intcode.debug := true

let start_state =
  let input = open_in "input.txt" in
  let r = Intcode.input_program input in
  close_in input;
  r

type fstate = {
  state: Intcode.state;
  address : int;
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
     | exception Queue.Empty -> k (-1); None
     | (x,y) -> k x; match run state.state with
       | Done | HaveOutput _ -> abort "expected another input instr\n"
       | WaitInput k -> k y; None)
  | HaveOutput o ->
    match state.output_buf with
    | None -> state.output_buf <- Some (o, None); None
    | Some (a, None) -> state.output_buf <- Some (a, Some o); None
    | Some (a, Some x) -> begin
        state.output_buf <- None;
        Some (a,(x,o))
      end

let boot_state i =
  let state = Intcode.make_state ~memsize:10_000 start_state in
  match run state with
  | Done | HaveOutput _ -> abort "bad boot\n"
  | WaitInput k -> k i; state

let states = Array.init 50 (fun i ->
    { state = boot_state i;
      address = i;
      input = Queue.create();
      output_buf = None; })

exception Finished of int

let rec run_all cur =
  let next = (cur+1) mod 50 in
  match run1 states.(cur) with
   | None -> run_all next
   | Some (a,(_,y as packet)) ->
     if a = 255 then y
     else begin
       Queue.add packet states.(a).input;
       run_all next
     end

let r = run_all 0

let () = Printf.printf "%d\n" r
