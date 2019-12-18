
(** IntCode V6.5

    debugging levels



 *)

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

let start_state =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  let r = List.map int_of_string (String.split_on_char ',' l) in
  Array.of_list r

let () = debug 3 "%d cells\n" (Array.length start_state)

type state = { mem : int array; mutable pc : int;
               mutable relative_base : int; }

type status =
  | WaitInput of (int -> unit)
  | HaveOutput of int
  | Done

let make_state () =
  let mem = Array.make 10_000 0 in
  Array.blit start_state 0 mem 0 (Array.length start_state);
  { mem;
    pc = 0;
    relative_base = 0;
  }

let incr_pc state = state.pc <- state.pc + 1

type mode = Position | Immediate | Relative

let get state ~mode =
  let imm = state.mem.(state.pc) in
  incr_pc state;
  let v = match mode with
    | Immediate -> imm
    | Position -> state.mem.(imm)
    | Relative -> state.mem.(imm + state.relative_base)
  in
  debug 2 "read %d\n" v;
  v

let set state ~mode v =
  let where = match mode with
    | Immediate -> abort "bad mode at pc %d\n" state.pc
    | Position -> state.mem.(state.pc)
    | Relative -> state.mem.(state.pc) + state.relative_base
  in
  debug 2 "setting %d <- %d\n" where v;
  state.mem.(where) <- v;
  incr_pc state

type ops = Exit | Add | Mult | Input | Output
         | JumpNZ | JumpZ | Lt | Eq | BaseOffset

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

let do_output state modes =
  Some (HaveOutput (getm state modes))

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

let do_exit _state _modes =
  Some Done

(* run until we need more input *)
let rec run state =
  let pc = state.pc in (* for error reporting *)
  debug 2 "prep to exec at %d\n" pc;
  let modes, code = parse_opcode state (get state ~mode:Immediate) in
  debug 2 "exec %s\n" (string_of_op code);
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
  | BaseOffset -> do_baseoffset
  | Exit -> do_exit
  in
  let status = exec state modes in
  abort_unless (!modes = []) "too many modes at pc %d\n" pc;
  match status with None -> run state | Some status -> status

(** INTCODE ENDS HERE *)

let rec read_firstline state acc =
  match run state with
  | Done | WaitInput _ -> abort "expected more output (first line)\n"
  | HaveOutput o -> match char_of_int o with
    | '\n' -> Array.of_list (List.rev acc)
    | c -> read_firstline state (c::acc)

let rec read_line state r i =
  match run state with
  | Done -> abort "expected more output (short line)\n"
  | WaitInput _ -> abort "no input to give\n"
  | HaveOutput o ->
    let c = char_of_int o in
    match c with
    | '\n' -> abort_unless (i = Array.length r) "bad output (non newline at end of line)\n"
    | _ ->
      (abort_unless (i < Array.length r) "bad output (too much in line)\n";
       r.(i) <- c;
       read_line state r (i+1))

(* the intcode program indicates the end of drawing with a double newline before halting *)
let rec read_lines state = function
  | [] -> read_lines state [read_firstline state []]
  | l :: _ as lines -> match run state with
    | Done -> abort "unexpected halt\n"
    | WaitInput _ -> abort "no input to give\n"
    | HaveOutput o ->
      let c = char_of_int o in
      if c = '\n' then (match run state with
          | Done -> Array.of_list (List.rev lines)
          | _ -> abort "expected halt\n")
      else begin
        let r = Array.make (Array.length l) c in
        read_line state r 1;
        read_lines state (r::lines)
      end

let lines = read_lines (make_state ()) []

let is_scaffold i j = lines.(i).(j) = '#'

let start =
  let exception Found of (int * int) in
  try Array.iteri (fun i line -> Array.iteri (fun j -> function
      | '^' -> raise (Found (i,j))
      | _ -> ()) line) lines; abort "start not found\n"
  with Found r -> r

type act = TurnL | TurnR | Continue

type direction = Up | Left | Right | Down

let move_dir (i,j) = function
  | Up -> (i-1,j)
  | Down -> (i+1,j)
  | Left -> (i,j-1)
  | Right -> (i,j+1)

let turn_left = function
  | Up -> Left
  | Left -> Down
  | Down -> Right
  | Right -> Up

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let safe_get (i,j) =
  try Some lines.(i).(j) with Invalid_argument _ -> None

let safe_scaffold p = match safe_get p with
  | None -> false
  | Some '#' -> true
  | Some _ -> false

let rec walk hist p dir =
  let p' = move_dir p dir in
  if safe_scaffold p' then walk (Continue::hist) p' dir
  else
    let d' = turn_left dir in
    let p' = move_dir p d' in
    if safe_scaffold p' then walk (Continue::TurnL::hist) p' d'
    else
      let d' = turn_right dir in
      let p' = move_dir p d' in
      if safe_scaffold p' then walk (Continue::TurnR::hist) p' d'
      else List.rev hist

let path = walk [] start Up

type compressed = L | R | C of int
let show_compressed = function
  | L -> "L"
  | R -> "R"
  | C i -> string_of_int i
let pp_compressed ch c = Format.fprintf ch "%s" (show_compressed c)

let rec compress acc i = function
  | [] -> List.rev (C i :: acc)
  | Continue :: rest -> compress acc (i+1) rest
  | TurnL :: rest -> compress (L :: C i :: acc) 0 rest
  | TurnR :: rest -> compress (R :: C i :: acc) 0 rest

let cpath = match compress [] 0 path with
  | C 0 :: rest -> rest
  | _ -> abort "unexpected compression\n"

let () = if debugging >= 1 then Format.printf "%a\n" (prlist pp_compressed "") cpath

(* hand solved from the above debug print *)
let inputs = [
  "A,B,A,B,C,C,B,A,B,C";
  "L,12,L,6,L,8,R,6";
  "L,8,L,8,R,4,R,6,R,6";
  "L,12,R,6,L,8";
  "n";
]

let () = abort_unless (List.for_all (fun s -> String.length s <= 20) inputs) "too long input"

let rec feed_input state s i =
  match run state with
  | Done -> abort "unexpected in feed_input\n"
  | HaveOutput _o -> (* it prints the map and prompts, ignore them *) feed_input state s i
  | WaitInput k ->
    let k c = k (int_of_char c) in
    if i = String.length s then k '\n'
    else (k (String.get s i); feed_input state s (i+1))

let rec feed_inputs state = function
  | [] -> ()
  | s :: rest -> feed_input state s 0; feed_inputs state rest

let state = make_state()
let () = state.mem.(0) <- 2

let () = feed_inputs state inputs

(* it prints the updated map at the end, ignore it. We save the last
   output since it's what we want. *)
let rec aux last = match run state with
  | Done -> last
  | WaitInput _ -> abort "unexpected at end\n"
  | HaveOutput o -> aux o

let res = aux 0

let () = Printf.printf "%d\n" res
