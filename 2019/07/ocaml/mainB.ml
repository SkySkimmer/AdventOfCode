
let debug m = if false then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let start_state =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  let r = List.map int_of_string (String.split_on_char ',' l) in
  Array.of_list r

let () = debug "%d cells\n" (Array.length start_state)

type state = { mem : int array; self : int; mutable pc : int;
               input : int Queue.t; }

let reset_state self =
  { mem = Array.copy start_state;
    self;
    pc = 0;
    input = Queue.create (); }

let states = Array.init 5 reset_state

let state_mod i = states.(i mod Array.length states)

let reset_states () = Array.iteri (fun self _ ->
    states.(self) <- reset_state self)
    states

let incr_pc state = state.pc <- state.pc + 1

type mode = Position | Immediate

let get state ~mode =
  let imm = state.mem.(state.pc) in
  incr_pc state;
  let v = match mode with
    | Immediate -> imm
    | Position -> state.mem.(imm)
  in
  debug "read %d\n" v;
  v

let set state ~mode v = debug "setting %d\n" v; match mode with
  | Immediate -> abort "bad mode at pc %d\n" state.pc
  | Position -> state.mem.(state.mem.(state.pc)) <- v; incr_pc state

type ops = Exit | Add | Mult | Input | Output
         | JumpNZ | JumpZ | Lt | Eq

let string_of_op = function
  | Exit -> "Exit"
  | Add -> "Add"
  | Mult -> "Mult"
  | Input -> "Input"
  | Output -> "Output"
  | JumpNZ -> "JumpNZ"
  | JumpZ -> "JumpZ"
  | Lt -> "Lt"
  | Eq -> "Eq"

let parse_instr state i =
  match i mod 100 with
  | 1 -> Add
  | 2 -> Mult
  | 3 -> Input
  | 4 -> Output
  | 5 -> JumpNZ
  | 6 -> JumpZ
  | 7 -> Lt
  | 8 -> Eq
  | 99 -> Exit
  | _ -> abort "Invalid opcode %d at pc %d: bad instruction\n" i state.pc

let parse_mode state = function
  | 0 -> Position
  | 1 -> Immediate
  | i -> abort "unknown mode %d at pc %d" i state.pc

let parse_modes state =
  let rec aux acc i =
    if i = 0 then List.rev acc
    else aux (parse_mode state (i mod 10) :: acc) (i / 10)
  in
  fun i -> aux [] (i/100)

let popmode modes = match !modes with
  | [] -> Position
  | mode :: tl -> modes := tl; mode

let getm state modes =
  get ~mode:(popmode modes) state

let setm state modes v =
  set state ~mode:(popmode modes) v

let parse_opcode state i =
  parse_modes state i, parse_instr state i

let do_add state modes =
  let a = getm state modes in
  let b = getm state modes in
  setm state modes (a + b)

let do_mult state modes =
  let a = getm state modes in
  let b = getm state modes in
  setm state modes (a * b)

let do_input state modes =
  let v = try Queue.take state.input
    with Queue.Empty -> abort "insufficient input at pc %d for %d\n" state.pc state.self
  in
  setm state modes v

let do_output state modes =
  let v = getm state modes in
  let target = (state_mod (state.self + 1)).input in
  Queue.add v target

let do_jumpnz state modes =
  let v = getm state modes in
  let target = getm state modes in
  if v <> 0 then state.pc <- target

let do_jumpz state modes =
  let v = getm state modes in
  let target = getm state modes in
  if v = 0 then state.pc <- target

let do_lt state modes =
  let a = getm state modes in
  let b = getm state modes in
  let v = if a < b then 1 else 0 in
  setm state modes v

let do_eq state modes =
  let a = getm state modes in
  let b = getm state modes in
  let v = if a = b then 1 else 0 in
  setm state modes v

let finish () =
  try Queue.take states.(0).input
  with Queue.Empty -> abort "missing final input"

let rec run i =
  let state = states.(i) in
  let pc = state.pc in (* for error reporting *)
  debug "prep to exec at %d in %d\n" pc i;
  let modes, code = parse_opcode state (get state ~mode:Immediate) in
  debug "exec %s\n" (string_of_op code);
  let modes = ref modes in
  let exec = match code with
  | Add -> do_add
  | Mult -> do_mult
  | Input -> do_input
  | Output -> do_output
  | JumpNZ -> do_jumpnz
  | JumpZ -> do_jumpz
  | Lt -> do_lt
  | Eq -> do_eq
  | Exit -> fun _ _ -> ()
  in
  exec state modes;
  abort_unless (!modes = []) "too many modes at pc %d\n" pc;
  if code = Exit && i+1 = Array.length states then finish ()
  else
    let i = if code = Exit || code = Output then (i+1) mod Array.length states else i in
    run i

let run_with coeffs =
  debug "RESET\n\n";
  reset_states ();
  List.iteri (fun i coeff -> debug "coeff %d in %d\n" coeff i;
               Queue.add coeff states.(i).input) coeffs;
  Queue.add 0 states.(0).input;
  run 0

let coeffs = List.init 5 (fun x -> x+5)

let rec all_insert acc stack x = function
  | [] -> (List.rev_append stack [x]) :: acc
  | y :: tl -> all_insert ((List.rev_append stack (x :: y :: tl)) :: acc) (y::stack) x tl

let rec permut = function
  | [] -> [[]]
  | x :: rest ->
    let rest = permut rest in
    List.flatten (List.map (fun p -> all_insert [] [] x p) rest)

let coeff_permut = permut coeffs

let all_runs = List.map run_with coeff_permut

let rec find_max best = function
  | [] -> best
  | x :: rest -> find_max (max x best) rest

let find_max = function
  | [] -> abort "no max in empty list"
  | x :: rest -> find_max x rest

let max = find_max all_runs

let () = Printf.printf "%d\n" max
