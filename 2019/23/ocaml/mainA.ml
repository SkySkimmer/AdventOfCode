open Utils

let start_state =
  let input = open_in "input.txt" in
  let r = Intcode.input_program input in
  close_in input;
  r

type fstate = {
  state : Intcode.state;
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
    | exception Queue.Empty ->
      k (-1);
      None
    | x, y ->
      k x;
      (match run state.state with
      | Done | HaveOutput _ -> abort "expected another input instr\n"
      | WaitInput k ->
        k y;
        None))
  | HaveOutput o ->
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
        input = Queue.create ();
        output_buf = None;
      })

exception Finished of int

let rec run_all cur =
  let next = (cur + 1) mod 50 in
  match run1 states.(cur) with
  | None -> run_all next
  | Some (a, ((_, y) as packet)) ->
    if a = 255 then y
    else begin
      Queue.add packet states.(a).input;
      run_all next
    end

let r = run_all 0

let () = Printf.printf "%d\n" r
