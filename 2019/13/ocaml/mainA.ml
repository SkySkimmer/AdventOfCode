(** IntCode V5.0

    No changes
*)

let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let start_state =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  let r = List.map int_of_string (String.split_on_char ',' l) in
  Array.of_list r

let () = debug "%d cells\n" (Array.length start_state)

type state = { mem : int array; mutable pc : int; mutable relative_base : int }

type status = WaitInput of (int -> unit) | HaveOutput of int | Done

let make_state () =
  let mem = Array.make 10_000 0 in
  Array.blit start_state 0 mem 0 (Array.length start_state);
  { mem; pc = 0; relative_base = 0 }

let incr_pc state = state.pc <- state.pc + 1

type mode = Position | Immediate | Relative

let get state ~mode =
  let imm = state.mem.(state.pc) in
  incr_pc state;
  let v =
    match mode with
    | Immediate -> imm
    | Position -> state.mem.(imm)
    | Relative -> state.mem.(imm + state.relative_base)
  in
  debug "read %d\n" v;
  v

let set state ~mode v =
  let where =
    match mode with
    | Immediate -> abort "bad mode at pc %d\n" state.pc
    | Position -> state.mem.(state.pc)
    | Relative -> state.mem.(state.pc) + state.relative_base
  in
  debug "setting %d <- %d\n" where v;
  state.mem.(where) <- v;
  incr_pc state

type ops =
  | Exit
  | Add
  | Mult
  | Input
  | Output
  | JumpNZ
  | JumpZ
  | Lt
  | Eq
  | BaseOffset

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
  | BaseOffset -> "BaseOffset"

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
  | 9 -> BaseOffset
  | 99 -> Exit
  | _ -> abort "Invalid opcode %d at pc %d: bad instruction\n" i state.pc

let parse_mode state = function
  | 0 -> Position
  | 1 -> Immediate
  | 2 -> Relative
  | i -> abort "unknown mode %d at pc %d" i state.pc

let parse_modes state =
  let rec aux acc i =
    if i = 0 then List.rev acc
    else aux (parse_mode state (i mod 10) :: acc) (i / 10)
  in
  fun i -> aux [] (i / 100)

let popmode modes =
  match !modes with
  | [] -> Position
  | mode :: tl ->
    modes := tl;
    mode

let getm state modes = get ~mode:(popmode modes) state

let setm state modes v = set state ~mode:(popmode modes) v

let parse_opcode state i = (parse_modes state i, parse_instr state i)

let do_add state modes =
  let a = getm state modes in
  let b = getm state modes in
  setm state modes (a + b);
  None

let do_mult state modes =
  let a = getm state modes in
  let b = getm state modes in
  setm state modes (a * b);
  None

let do_input state modes =
  let mode = popmode modes in
  Some (WaitInput (set state ~mode))

let do_output state modes = Some (HaveOutput (getm state modes))

let do_jumpnz state modes =
  let v = getm state modes in
  let target = getm state modes in
  if v <> 0 then state.pc <- target;
  None

let do_jumpz state modes =
  let v = getm state modes in
  let target = getm state modes in
  if v = 0 then state.pc <- target;
  None

let do_lt state modes =
  let a = getm state modes in
  let b = getm state modes in
  let v = if a < b then 1 else 0 in
  setm state modes v;
  None

let do_eq state modes =
  let a = getm state modes in
  let b = getm state modes in
  let v = if a = b then 1 else 0 in
  setm state modes v;
  None

let do_baseoffset state modes =
  let v = getm state modes in
  state.relative_base <- state.relative_base + v;
  None

let do_exit _state _modes = Some Done

(* run until we need more input *)
let rec run state =
  let pc = state.pc in
  (* for error reporting *)
  debug "prep to exec at %d\n" pc;
  let modes, code = parse_opcode state (get state ~mode:Immediate) in
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
    | BaseOffset -> do_baseoffset
    | Exit -> do_exit
  in
  let status = exec state modes in
  abort_unless (!modes = []) "too many modes at pc %d\n" pc;
  match status with None -> run state | Some status -> status

let () = debug "%d base cells\n" (Array.length start_state)

let rec get_output state =
  match run state with
  | Done -> []
  | WaitInput _ -> abort "no input to give!\n"
  | HaveOutput o -> o :: get_output state

module PMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

type tile = Empty | Wall | Block | Paddle | Ball

let parse_tile = function
  | 0 -> Empty
  | 1 -> Wall
  | 2 -> Block
  | 3 -> Paddle
  | 4 -> Ball
  | c -> abort "unknown tile type %d\n" c

let rec interp acc = function
  | [] -> acc
  | x :: y :: c :: rest ->
    let acc =
      PMap.union
        (fun _ _ _ -> (abort "double write!\n" : _ option))
        acc
        (PMap.singleton (x, y) (parse_tile c))
    in
    interp acc rest
  | _ -> abort "incorrect output count!\n"

let points = interp PMap.empty (get_output (make_state ()))

let count =
  PMap.fold (fun _ c acc -> if c = Block then acc + 1 else acc) points 0

let () = Printf.printf "%d\n" count
