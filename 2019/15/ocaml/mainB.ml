
(** IntCode V6.0

    No changes



 *)

let debugging = match Sys.argv with
  | [|_;"-debug"|] -> true
  | _ -> false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let start_state =
  let input = open_in "input.txt" in
  let l = input_line input in
  close_in input;
  let r = List.map int_of_string (String.split_on_char ',' l) in
  Array.of_list r

(* let () = debug "%d cells\n" (Array.length start_state) *)

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
  (* debug "read %d\n" v; *)
  v

let set state ~mode v =
  let where = match mode with
    | Immediate -> abort "bad mode at pc %d\n" state.pc
    | Position -> state.mem.(state.pc)
    | Relative -> state.mem.(state.pc) + state.relative_base
  in
  (* debug "setting %d <- %d\n" where v; *)
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
  (* debug "prep to exec at %d\n" pc; *)
  let modes, code = parse_opcode state (get state ~mode:Immediate) in
  (* debug "exec %s\n" (string_of_op code); *)
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

(* let () = debug "%d base cells\n" (Array.length start_state) *)

module Point = struct
  type t = int * int

  (* compare y first *)
  let compare (x,y) (x',y') = compare (y,x) (y',x')

end

module PMap = Map.Make(Point)

type tile = Empty | Wall | Goal

let parse_tile = function
  | 0 -> Wall
  | 1 -> Empty
  | 2 -> Goal
  | c -> abort "unknown tile type %d\n" c

type direction = Up | Down | Left | Right

let com_of_dir = function
  | Up -> 1
  | Down -> 2
  | Left -> 3
  | Right -> 4

type gamestate = {
  tiles : tile PMap.t;
  self : Point.t;
  goal : Point.t option;
  minx : int; miny: int;
  work : Point.t list;
  path : direction list;
}

let fillto minx (x,y) (x',y') =
  abort_unless (y < y' || (y = y' && x <= x')) "bad order\n";
  if y < y' then begin
    print_string (String.make (y'-y) '\n');
    if x' > minx then print_string (String.make (x'-minx) ' ')
  end
  else if x' - 1 > x then print_string (String.make (x'-x-1) ' ')

let pr_tile = function
  | Empty -> "."
  | Wall -> "W"
  | Goal -> "G"

let print1 ~self = function
  | Empty -> print_char (if self then 'X' else '.')
  | Wall -> abort_unless (not self) "self can't be on wall"; print_char 'W'
  | Goal -> print_char (if self then 'g' else 'G')

let has_printed = ref false

let suppress_output = true

let draw_game game =
  if not suppress_output then begin
    print_string "==========================\n";
    let _,_ =
      PMap.fold (fun pos' tile pos ->
          fillto game.minx pos pos';
          if pos' = (0,0) then print_char 'O' else print1 ~self:(pos' = game.self) tile;
          pos')
        game.tiles
        (game.minx,game.miny)
    in
    print_string "\n";
    flush stdout
  end

let move_pos (x,y) = function
  | Up -> (x,y-1)
  | Down -> (x,y+1)
  | Left -> (x-1,y)
  | Right -> (x+1,y)

let turn = function
  | Up -> Left
  | Down -> Right
  | Left -> Down
  | Right -> Up

let pr_move = function
  | Up -> "U"
  | Down -> "D"
  | Left -> "L"
  | Right -> "R"

let rand = Random.init 0

let neighbours_dir (x,y) =
  [(x,y-1),Up;(x,y+1),Down;(x-1,y),Left;(x+1,y),Right]

module PSet = Set.Make(Point)

let make_path game target =
  let work = Queue.create () in
  let () = Queue.add (game.self,[]) work in
  let rec find visited =
    let p, path = Queue.take work in
    let visited' = PSet.add p visited in
    if visited == visited' then find visited
    else if p = target then List.rev path
    else match PMap.find_opt p game.tiles with
      | None | Some Wall -> find visited'
      | Some _ ->
        List.iter (fun (p,d) -> Queue.add (p,d::path) work) (neighbours_dir p);
        find visited'
  in
  find PSet.empty

let rec prlist pr sep ch = function
  | [] -> ()
  | [x] -> pr ch x
  | x :: rest -> pr ch x; Printf.fprintf ch sep; prlist pr sep ch rest

let pr_path ch path = prlist (fun ch d -> Printf.fprintf ch "%s" (pr_move d)) "" ch path

let rec next_target game work =
  match work with
  | [] -> `Done {game with work}
  | w :: rest ->
    if PMap.mem w game.tiles then next_target game rest
    else begin
      debug "next target %d,%d\n" (fst w) (snd w);
      let path = make_path game w in
      debug "path %a\n" pr_path path;
      `Continue {game with work; path}
    end

let neighbours p = List.map fst (neighbours_dir p)

let get_move game =
  match game.path with
  | [] -> abort "bad path\n"
  | [_] ->
    abort_unless (game.work <> []) "bad work\n";
    let work = List.append (neighbours game.self) (List.tl game.work) in
    next_target game work
  | _ :: rest -> `Continue {game with path=rest}

let update_game game tile =
  let self' = move_pos game.self (List.hd game.path) in
  debug "at %d,%d got %s moved %s\n" (fst game.self) (snd game.self)
    (pr_tile tile) (pr_move (List.hd game.path));
  let self = if tile = Wall then game.self else self' in
  let tiles = PMap.add self' tile game.tiles in
  let minx = min game.minx (fst self') in
  let miny = min game.miny (snd self') in
  let goal = if tile = Goal then Some self else game.goal in
  let game = {game with self;tiles;minx;miny;goal} in
  get_move game

let rec search state game =
  match run state with
  | Done -> abort "unexpected halt"
  | WaitInput k ->
    k (com_of_dir (List.hd game.path));
    search state game
  | HaveOutput o -> match update_game game (parse_tile o) with
    | `Done res -> draw_game game; res
    | `Continue game ->
      draw_game game;
      search state game

let start_game =
  { tiles = PMap.empty;
    self = (0,1); goal = None;
    work = [0,0];
    path = [Up]; (* start by fake Up movement from (0,1) *)
    minx = 0; miny = 0; }

let start_game = match update_game start_game Empty with
  | `Continue game -> game
  | `Done _ -> assert false

let game = search (make_state ()) start_game
let goal = match game.goal with
  | None -> abort "no goal found\n"
  | Some goal -> goal

let work = Queue.create ()
let () = Queue.add (goal,0) work

let rec spread maxcost visited =
  match Queue.take work with
  | exception Queue.Empty -> maxcost
  | (p,cost) ->
    let visited' = PSet.add p visited in
    if visited == visited' then spread maxcost visited
    else match PMap.find_opt p game.tiles with
      | None | Some Wall -> spread maxcost visited'
      | Some _ ->
        List.iter (fun p -> Queue.add (p,cost+1) work) (neighbours p);
        let maxcost = max cost maxcost in
        spread maxcost visited'

let maxcost = spread 0 PSet.empty

let () = Printf.printf "%d\n" maxcost
