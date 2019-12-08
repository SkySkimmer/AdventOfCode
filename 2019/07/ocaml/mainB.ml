
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

type status = Ready | Waiting | Done

type state = { mem : int array; pid : int; mutable pc : int; mutable status : status;
               input : int Queue.t; mutable last_output : int option; }

let reset_state pid =
  { mem = Array.copy start_state;
    pid;
    status = Ready;
    pc = 0;
    input = Queue.create ();
    last_output = None;}

let states = Array.init 5 reset_state

let state_mod i = states.(i mod Array.length states)

let reset_states () = Array.iteri (fun pid _ ->
    states.(pid) <- reset_state pid)
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
  match Queue.take state.input with
  | exception Queue.Empty -> ignore (popmode modes); state.status <- Waiting; state.pc <- state.pc - 1
  | v -> setm state modes v

let do_output state modes =
  let v = getm state modes in
  state.last_output <- Some v;
  let target = state_mod (state.pid + 1) in
  Queue.add v target.input;
  target.status <- Ready

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

let do_exit state _modes =
  state.status <- Done

let finish () =
  try Queue.take states.(0).input
  with Queue.Empty -> abort "missing final input"

(* run until we need more input *)
let rec run state =
  let pc = state.pc in (* for error reporting *)
  debug "prep to exec at %d in %d\n" pc state.pid;
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
  | Exit -> do_exit
  in
  exec state modes;
  abort_unless (!modes = []) "too many modes at pc %d\n" pc;
  if state.status = Ready then run state

let find_ready () =
  let rec aux i =
    if states.(i).status = Ready then states.(i)
    else if i = Array.length states - 1 then abort "no ready state"
    else aux (i+1)
  in
  aux 0

let rec run_all () =
  let state = find_ready () in
  run state;
  if state.status = Done && state.pid = Array.length states - 1
  then match state.last_output with
    | Some out -> out
    | None -> abort "no output from final process!"
  else run_all ()

let run_with coeffs =
  debug "RESET\n\n";
  reset_states ();
  List.iteri (fun i coeff -> debug "coeff %d in %d\n" coeff i;
               Queue.add coeff states.(i).input) coeffs;
  Queue.add 0 states.(0).input;
  run_all ()

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
