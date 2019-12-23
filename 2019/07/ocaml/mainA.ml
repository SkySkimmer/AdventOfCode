let debug m = if false then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let start_state =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  let r = List.map int_of_string (String.split_on_char ',' l) in
  Array.of_list r

type state = {
  mem : int array;
  mutable pc : int;
  mutable input : int list;
  mutable output : int list;
}

let state = { mem = Array.copy start_state; pc = 0; input = []; output = [] }

let reset_state input =
  Array.blit start_state 0 state.mem 0 (Array.length start_state);
  state.pc <- 0;
  state.input <- input;
  state.output <- []

let incr_pc () = state.pc <- state.pc + 1

type mode = Position | Immediate

let get ~mode () =
  let imm = state.mem.(state.pc) in
  incr_pc ();
  match mode with Immediate -> imm | Position -> state.mem.(imm)

let set ~mode v =
  match mode with
  | Immediate -> abort "bad mode at pc %d\n" state.pc
  | Position ->
    state.mem.(state.mem.(state.pc)) <- v;
    incr_pc ()

type ops = Exit | Add | Mult | Input | Output | JumpNZ | JumpZ | Lt | Eq

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

let parse_instr i =
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

let parse_mode = function
  | 0 -> Position
  | 1 -> Immediate
  | i -> abort "unknown mode %d at pc %d" i state.pc

let parse_modes =
  let rec aux acc i =
    if i = 0 then List.rev acc else aux (parse_mode (i mod 10) :: acc) (i / 10)
  in
  fun i -> aux [] (i / 100)

let popmode modes =
  match !modes with
  | [] -> Position
  | mode :: tl ->
    modes := tl;
    mode

let getm modes () = get ~mode:(popmode modes) ()

let setm modes v = set ~mode:(popmode modes) v

let parse_opcode i = (parse_modes i, parse_instr i)

let do_add modes =
  let a = getm modes () in
  let b = getm modes () in
  setm modes (a + b)

let do_mult modes =
  let a = getm modes () in
  let b = getm modes () in
  setm modes (a * b)

let do_input modes =
  let v =
    match state.input with
    | v :: tl ->
      state.input <- tl;
      v
    | [] -> abort "insufficient input at pc %d\n" state.pc
  in
  setm modes v

let do_output modes =
  let v = getm modes () in
  state.output <- v :: state.output

let do_jumpnz modes =
  let v = getm modes () in
  let target = getm modes () in
  if v <> 0 then state.pc <- target

let do_jumpz modes =
  let v = getm modes () in
  let target = getm modes () in
  if v = 0 then state.pc <- target

let do_lt modes =
  let a = getm modes () in
  let b = getm modes () in
  let v = if a < b then 1 else 0 in
  setm modes v

let do_eq modes =
  let a = getm modes () in
  let b = getm modes () in
  let v = if a = b then 1 else 0 in
  setm modes v

let rec run () =
  let pc = state.pc in
  (* for error reporting *)
  debug "prep to exec at %d\n" pc;
  let modes, code = parse_opcode (get ~mode:Immediate ()) in
  debug "exec %s\n" (string_of_op code);
  let modes = ref modes in
  let exec =
    match code with
    | Add -> do_add
    | Mult -> do_mult
    | Input -> do_input
    | Output -> do_output
    | JumpNZ -> do_jumpnz
    | JumpZ -> do_jumpz
    | Lt -> do_lt
    | Eq -> do_eq
    | Exit -> ignore
  in
  exec modes;
  abort_unless (!modes = []) "too many modes at pc %d\n" pc;
  if code = Exit then () else run ()

let run input =
  reset_state input;
  run ();
  match state.output with
  | [ v ] -> v
  | [] -> abort "no output"
  | _ -> abort "unexpected output"

let max_coeff = 4

let coeffs = List.init (max_coeff + 1) (fun x -> x)

let rec all_runs i data =
  if i = max_coeff + 1 then
    List.map
      (fun (coeffs, history, output) ->
        abort_unless (coeffs = []) "unexpected remaining coeffs";
        (history, output))
      data
  else
    let data =
      List.flatten
        (List.map
           (fun (coeffs, history, output) ->
             List.map
               (fun coeff ->
                 let output = run [ coeff; output ] in
                 let coeffs = List.filter (fun c' -> c' <> coeff) coeffs in
                 (coeffs, coeff :: history, output))
               coeffs)
           data)
    in
    all_runs (i + 1) data

let all_runs = all_runs 0 [ (coeffs, [], 0) ]

let rec find_max best = function
  | [] -> best
  | (_, x) :: rest -> find_max (max x best) rest

let max =
  match all_runs with
  | [] -> abort "missing runs"
  | (_, output) :: rest -> find_max output rest

let () = debug "%d cells\n" (Array.length start_state)

let () = Printf.printf "%d\n" max
