(** IntCode V5.0

    No changes

    Ball game: change always_autoinput and suppress_drawing (later in
   this file) to enable manual play.

    Automatic strategy is to always move the paddle towards the ball.

    When redrawing, screen size is hardcoded near draw_game.
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

module PMap = Map.Make (struct
  type t = int * int

  (* compare y first *)
  let compare (x, y) (x', y') = compare (y, x) (y', x')
end)

type tile = Empty | Wall | Block | Paddle | Ball

let parse_tile = function
  | 0 -> Empty
  | 1 -> Wall
  | 2 -> Block
  | 3 -> Paddle
  | 4 -> Ball
  | c -> abort "unknown tile type %d\n" c

type gamestate = {
  tiles : tile PMap.t;
  paddle : int * int;
  ball : int * int;
  score : int;
  buffer : int list; (* pending output instructions *)
}

let set_tile x y c tiles = PMap.add (x, y) c tiles

let process_buffer game =
  match game.buffer with
  (* buffer is stack: LIFO! *)
  | [ score; 0; -1 ] -> { game with score; buffer = [] }
  | [ c; y; x ] ->
    abort_unless (0 <= x && 0 <= y) "negative coords\n";
    let c = parse_tile c in
    {
      game with
      tiles = set_tile x y c game.tiles;
      paddle = (if c = Paddle then (x, y) else game.paddle);
      ball = (if c = Ball then (x, y) else game.ball);
      buffer = [];
    }
  | _ -> game

let fillto (x, y) (x', y') =
  abort_unless (y < y' || (y = y' && x <= x')) "bad order\n";
  if y < y' then begin
    print_string (String.make (y' - y) '\n');
    if x' > 0 then print_string (String.make x' ' ')
  end
  else if x' - 1 > x then print_string (String.make (x' - x - 1) ' ')

let print1 = function
  | Empty -> print_char ' '
  | Wall -> print_char 'W'
  | Block -> print_char 'B'
  | Paddle -> print_string "─"
  | Ball -> print_string "◆"

let screenx = 43

let screeny = 22

let has_printed = ref false

let draw_game game =
  if not !has_printed then (
    has_printed := true;
    print_string "\027[2J");
  let score = string_of_int game.score in
  let lead_chars = (screenx - String.length score - 2) / 2 in
  print_string (String.make lead_chars '=');
  print_char ' ';
  print_string score;
  print_char ' ';
  print_string
    (String.make (screenx - lead_chars - String.length score - 2) '=');
  print_newline ();
  let _, _ =
    PMap.fold
      (fun pos' tile pos ->
        fillto pos pos';
        print1 tile;
        pos')
      game.tiles (0, 0)
  in
  print_string "\n";
  flush stdout

type command = Left | Neutral | Right

let comm_to_int = function Left -> -1 | Right -> 1 | Neutral -> 0

let int_to_comm = function
  | -1 -> Left
  | 0 -> Neutral
  | 1 -> Right
  | i -> abort "unknown command %d\n" i

(* put input here to run without manual intervention *)
let preinput = ref []

(* record input (LIFO) *)
let input_hist = ref []

let autoinput game =
  let px = fst game.paddle
  and bx = fst game.ball in
  int_to_comm (compare bx px)

let rec read_input game =
  let c = input_char stdin in
  match c with
  | '\n' -> autoinput game
  | 'L' -> Left
  | 'R' -> Right
  | ' ' -> Neutral
  | _ -> read_input game

let always_autoinput = true

let suppress_drawing = true

let get_input game =
  match !preinput with
  | i :: rest ->
    preinput := rest;
    i
  | [] ->
    if always_autoinput then begin
      (* 60fps (in manual input mode no need to sleep as keyboard repeat
         doesn't go that fast)
      *)
      if not suppress_drawing then Unix.sleepf (1. /. 60.);
      autoinput game
    end
    else read_input game

let rec run_game state game =
  match run state with
  | Done ->
    if not suppress_drawing then draw_game game;
    game.score
  | WaitInput k ->
    if not suppress_drawing then draw_game game;
    let i = get_input game in
    input_hist := i :: !input_hist;
    k (comm_to_int i);
    run_game state game
  | HaveOutput o ->
    let game = { game with buffer = o :: game.buffer } in
    let game = process_buffer game in
    run_game state game

let score =
  let state = make_state () in
  state.mem.(0) <- 2;
  (* insert quarters *)
  let game =
    {
      tiles = PMap.empty;
      paddle = (0, 0);
      ball = (0, 0);
      score = 0;
      buffer = [];
    }
  in
  let score = run_game state game in
  score

let () = Printf.printf "DONE %d\n" score
