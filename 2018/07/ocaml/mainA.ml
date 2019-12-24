open Utils.Util

let atoi s =
  abort_unless (String.length s = 1) "bad parse (elt len)\n";
  let c = s.[0] in
  abort_unless ('A' <= c && c <= 'Z') "bad parse (not capital)\n";
  int_of_char c - int_of_char 'A'

let parse_one line =
  Scanf.sscanf line "Step %s must be finished before step %s can begin."
    (fun a b -> (atoi a, atoi b))

let rules =
  let input = open_in "../input.txt" in
  let lines = input_lines input in
  close_in input;
  List.map parse_one lines

(* The state is a list with an element for each step (A to Z). Each
   element is the number of the step together with a bitfield where
   bit N means step N must be done before the current step (so 0 means
   always ready). Steps are removed when done. *)
let getbit i b = (i lsr b) mod 2 <> 0

let setbit i b = if getbit i b then i else i + (1 lsl b)

let ready ~finished ~needed = needed land finished = needed

let rules =
  let a = Array.make 26 0 in
  List.iter (fun (needed, goal) -> a.(goal) <- setbit a.(goal) needed) rules;
  Array.to_list (Array.mapi (fun i v -> (i, v)) a)

let rec find_next_aux ~finished stack = function
  | [] -> abort "could not find next step\n"
  | (who, needed) :: rest when ready ~finished ~needed ->
    (who, List.rev_append stack rest)
  | elt :: rest -> find_next_aux ~finished (elt :: stack) rest

let find_next ~finished todo = find_next_aux ~finished [] todo

let rec find_all ~finished acc todo =
  if todo = [] then List.rev acc
  else
    let this, todo = find_next ~finished todo in
    let finished = setbit finished this in
    find_all ~finished (this :: acc) todo

let work = find_all ~finished:0 [] rules

let () =
  List.iter
    (fun this ->
      let c = char_of_int (this + int_of_char 'A') in
      print_char c)
    work;
  print_newline ()
