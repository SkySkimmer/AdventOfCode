open Utils.Util

let () =
  if Array.length Sys.argv <> 2 then abort "usage: %s PROGRAM\n" Sys.argv.(0)

let prog = Sys.argv.(1)

let prog =
  try
    let f = open_in prog in
    let r = Intcode.input_program f in
    close_in f;
    r
  with Sys_error e -> abort "Could not open %s: %s\n" prog e

let state = Intcode.make_state ~memsize:10_000 prog

let string_of_output o =
  if o < 128 then String.make 1 (char_of_int o) else string_of_int o

let rec main () =
  match Intcode.run state with
  | Done -> flush stdout
  | HaveOutput o ->
    print_string (string_of_output o);
    main ()
  | WaitInput k ->
    flush stdout;
    let c = input_char stdin in
    k (int_of_char c);
    main ()

let () = main ()
