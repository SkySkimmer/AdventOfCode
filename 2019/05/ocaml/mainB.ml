
let debug m = if false then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m


(* The version in mainA fails to parse the last int. Apparently that's
   not a problem for part A. *)
let state =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  let r = List.map int_of_string (String.split_on_char ',' l) in
  Array.of_list r

let pc = ref 0

type mode = Position | Immediate
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

let get ~mode () =
  let imm = state.(!pc) in
  incr pc;
  let v = match mode with
    | Immediate -> imm
    | Position -> state.(imm)
  in
  debug "load %d\n" v;
  v

let set ~mode v = debug "set %d\n" v; match mode with
  | Immediate -> abort "bad mode at pc %d\n" !pc
  | Position -> state.(state.(!pc)) <- v; incr pc

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
  | _ -> abort "Invalid opcode %d at pc %d: bad instruction\n" i !pc

let parse_mode = function
  | 0 -> Position
  | 1 -> Immediate
  | i -> abort "unknown mode %d at pc %d" i !pc

let parse_modes =
  let rec aux acc i =
    if i = 0 then List.rev acc
    else aux (parse_mode (i mod 10) :: acc) (i / 10)
  in
  fun i -> aux [] (i/100)

let popmode modes = match !modes with
  | [] -> Position
  | mode :: tl -> modes := tl; mode

let getm modes () =
  get ~mode:(popmode modes) ()

let setm modes v =
  set ~mode:(popmode modes) v

let parse_opcode i =
  parse_modes i, parse_instr i

let do_add modes =
  let a = getm modes () in
  let b = getm modes () in
  setm modes (a + b)

let do_mult modes =
  let a = getm modes () in
  let b = getm modes () in
  setm modes (a * b)

let do_input modes =
  let v = 5 in
  setm modes v

let do_output modes =
  let v = getm modes () in
  Printf.printf "%d\n" v

let do_jumpnz modes =
  let v = getm modes () in
  let target = getm modes () in
  if v <> 0 then pc := target

let do_jumpz modes =
  let v = getm modes () in
  let target = getm modes () in
  if v = 0 then pc := target

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
  let pc = !pc in (* for error reporting *)
  debug "prep to exec at %d\n" pc;
  let modes, code = parse_opcode (get ~mode:Immediate ()) in
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
  | Exit -> ignore
  in
  exec modes;
  abort_unless (!modes = []) "too many modes at pc %d\n" pc;
  if code = Exit then ()
  else run ()

let () = debug "%d cells\n" (Array.length state)

let () = run ()
