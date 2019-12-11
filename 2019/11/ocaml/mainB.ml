
(** IntCode V4.5

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
  debug "read %d\n" v;
  v

let set state ~mode v =
  let where = match mode with
    | Immediate -> abort "bad mode at pc %d\n" state.pc
    | Position -> state.mem.(state.pc)
    | Relative -> state.mem.(state.pc) + state.relative_base
  in
  debug "setting %d <- %d\n" where v;
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
  debug "prep to exec at %d\n" pc;
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
  | BaseOffset -> do_baseoffset
  | Exit -> do_exit
  in
  let status = exec state modes in
  abort_unless (!modes = []) "too many modes at pc %d\n" pc;
  match status with None -> run state | Some status -> status

let () = debug "%d base cells\n" (Array.length start_state)

module PMap = Map.Make(struct type t = int * int
    (* we need lex order on y first *)
    let compare (x,y) (x',y') = compare (y,x) (y',x')
  end)

type direction = Up | Right | Down | Left
type turn = TurnLeft | TurnRight

type robot = { state: state; pos : int * int; dir : direction }

let do_turn robot turn =
  let turn = match turn with
    | 0 -> TurnLeft
    | 1 -> TurnRight
    | _ -> abort "unexpected turn value %d\n" turn
  in
  let dir = match robot.dir, turn with
    | Up, TurnLeft | Down, TurnRight -> Left
    | Right, TurnLeft | Left, TurnRight -> Up
    | Down, TurnLeft | Up, TurnRight -> Right
    | Left, TurnLeft | Right, TurnRight -> Down
  in
  { robot with dir }

let on_fst f (x,y) = f x, y
let on_snd f (x,y) = x, f y

let move robot =
  let f = match robot.dir with
    | Up -> on_snd pred
    | Down -> on_snd succ
    | Right -> on_fst succ
    | Left -> on_fst pred
  in
  let pos = f robot.pos in
  { robot with pos }

let turn_and_move robot turn =
  let robot = do_turn robot turn in
  move robot

(* run until done or output instr *)
let rec robot_run seen robot =
  match run robot.state with
  | Done -> None
  | WaitInput k ->
    let color = try PMap.find robot.pos seen with Not_found -> 0 in
    k color;
    robot_run seen robot
  | HaveOutput o -> Some o

let rec robot_paint seen robot =
  match robot_run seen robot with
  | None -> seen
  | Some col ->
    let seen = PMap.add robot.pos col seen in
    robot_move seen robot

and robot_move seen robot =
  match robot_run seen robot with
  | None -> abort "unexpected halt between paint and move"
  | Some turn ->
    let robot = turn_and_move robot turn in
    robot_paint seen robot

let new_robot () =
  { state = make_state(); pos = (0,0); dir = Up }

let seen = robot_paint (PMap.singleton (0,0) 1) (new_robot ())

let painted = PMap.bindings seen

let minx = List.fold_left (fun minx ((x,_),_) -> min minx x) 0 painted

let print1 = function
  | 0 -> print_string "█"
  | 1 -> print_string " "
  | color -> abort "unexpected color %d\n" color

let fillto (x,y) (x',y') =
  abort_unless (y < y' || (y = y' && x <= x')) "bad order\n";
  if y < y' then begin
    print_string (String.make (y'-y) '\n');
    if x' > minx then print_string (String.make (x'-minx) ' ')
  end
  else if x' - 1 > x then print_string (String.make (x'-x-1) ' ')

let rec print pos = function
  | [] -> print_newline()
  | (pos',color) :: rest ->
    fillto pos pos';
    print1 color;
    print pos' rest

let () = match painted with
  | [] -> abort "no paint!\n"
  | ((_x,y as pos),color) :: rest ->
    fillto (minx,y) pos;
    print1 color;
    print pos rest
