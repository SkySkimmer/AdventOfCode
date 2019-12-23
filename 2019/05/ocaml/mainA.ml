let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let rec parse ch acc =
  match Scanf.bscanf ch "%d" (fun d -> d) with
  | n ->
    (match Scanf.bscanf ch "," () with
    | exception _ -> List.rev acc
    | () -> parse ch (n :: acc))

let state =
  let input = Scanf.Scanning.open_in "input.txt" in
  let r = parse input [] in
  Scanf.Scanning.close_in input;
  Array.of_list r

let pc = ref 0

type mode = Position | Immediate

type ops = Exit | Add | Mult | Input | Output

let get ~mode () =
  let imm = state.(!pc) in
  incr pc;
  match mode with Immediate -> imm | Position -> state.(imm)

let set ~mode v =
  match mode with
  | Immediate -> abort "bad mode at pc %d\n" !pc
  | Position ->
    state.(state.(!pc)) <- v;
    incr pc

let parse_instr i =
  match i mod 100 with
  | 1 -> Add
  | 2 -> Mult
  | 3 -> Input
  | 4 -> Output
  | 99 -> Exit
  | _ -> abort "Invalid opcode %d at pc %d: bad instruction\n" i !pc

let parse_mode = function
  | 0 -> Position
  | 1 -> Immediate
  | i -> abort "unknown mode %d at pc %d" i !pc

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
  let v = 1 in
  setm modes v

let do_output modes =
  let v = getm modes () in
  Printf.printf "%d\n" v

let rec run () =
  let modes, code = parse_opcode (get ~mode:Immediate ()) in
  let modes = ref modes in
  let () =
    match code with
    | Add -> do_add modes
    | Mult -> do_mult modes
    | Input -> do_input modes
    | Output -> do_output modes
    | Exit -> ()
  in
  abort_unless (!modes = []) "too many modes at pc %d\n" !pc;
  if code = Exit then () else run ()

let () = run ()
