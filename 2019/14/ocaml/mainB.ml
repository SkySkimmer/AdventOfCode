let debugging = match Sys.argv with [| _; "-debug" |] -> true | _ -> false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

module SMap = Map.Make (String)

type rule = { produces : int; ingredients : int SMap.t }

let parse_one s =
  match String.split_on_char ' ' s with
  | [ count; what ] -> (what, int_of_string count)
  | _ -> abort "unexpected input\n"

let rec parse_components acc = function
  | [ Str.Text lastin; Delim " => "; Text s ] ->
    let lastin, lastinc = parse_one lastin in
    let acc = SMap.add lastin lastinc acc in
    let s, i = parse_one s in
    (s, { produces = i; ingredients = acc })
  | Text source :: Delim ", " :: rest ->
    let what, c = parse_one source in
    let acc = SMap.add what c acc in
    parse_components acc rest
  | _ -> abort "unexpected input\n"

let parse_line l =
  let components = Str.full_split (Str.regexp "\\(, \\)\\|\\( => \\)") l in
  parse_components SMap.empty components

let rules =
  let input = open_in "input.txt" in
  let rec read_in acc =
    match input_line input with
    | exception _ -> acc
    | "\n" -> acc
    | l ->
      let src, rule = parse_line l in
      read_in (SMap.add src rule acc)
  in
  let r = read_in SMap.empty in
  close_in input;
  r

let () = debug "%d\n" (SMap.cardinal rules)

let fuel = "FUEL"

let ore = "ORE"

let rec get_ore cost spares = function
  | [] -> (cost, spares)
  | (target, count) :: rest ->
    if target = ore then get_ore (cost + count) spares rest
    else
      let rest = ref rest in
      let spares =
        SMap.update target
          (fun spare ->
            let spare = match spare with Some s -> s | None -> 0 in
            if spare > count then Some (spare - count)
            else if spare = count then None
            else
              (* spare < count *)
              let rule = SMap.find target rules in
              let count = count - spare in
              let times = count / rule.produces in
              let times =
                if times * rule.produces = count then times else times + 1
              in
              rest :=
                SMap.fold
                  (fun what cost rest -> (what, cost * times) :: rest)
                  rule.ingredients !rest;
              Some ((times * rule.produces) - count))
          spares
      in
      get_ore cost spares !rest

let cost1, _spares1 = get_ore 0 SMap.empty [ (fuel, 1) ]

let trillion = 1_000_000_000_000

let rec eat_spares spares rem produced =
  let cost, spares = get_ore 0 spares [ (fuel, 1) ] in
  if cost > rem then produced else eat_spares spares (rem - cost) (produced + 1)

(* binary search for something between bot and top satisfying pred *)
let rec search pred bot top =
  if bot = top then begin
    abort_unless (pred bot) "couldn't find in search";
    bot
  end
  else if bot + 1 = top then if pred top then top else bot
  else
    let mid = ((top - bot) / 2) + bot in
    debug "search %d <= %d <= %d\n" bot mid top;
    if pred mid then search pred mid top else search pred bot mid

let produced =
  (* we can get at least [trillion / cost1] fuel *)
  let base = trillion / cost1 in
  (* we can't get more than [trillion] *)
  let top = trillion in
  search
    (fun x ->
      let costx, _ = get_ore 0 SMap.empty [ (fuel, x) ] in
      costx <= trillion)
    base top

let () = Printf.printf "%d\n" produced
