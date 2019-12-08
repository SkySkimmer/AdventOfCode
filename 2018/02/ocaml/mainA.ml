
let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

module CMap = Map.Make(struct type t = char let compare = compare end)

let osucc = function
  | None -> Some 1
  | Some n -> Some (n+1)

let rec count acc l i =
  if i = String.length l then acc
  else
    let acc = CMap.update (String.get l i) osucc acc in
    count acc l (i+1)

let int_of_bool = function true -> 1 | false -> 0

let count l =
  let c = count CMap.empty l 0 in
  let dubs, trips = CMap.fold (fun _ c (dubs,trips) -> dubs || c = 2, trips || c = 3)
      c (false,false)
  in
  int_of_bool dubs, int_of_bool trips

let rec read_all ch (dubs,trips) =
  match input_line ch with
  | exception End_of_file -> dubs, trips
  | l ->
    let ndubs,ntrips = count l in
    read_all ch (dubs+ndubs,trips+ntrips)

let res =
  let input = open_in "input.txt" in
  let dubs,trips = read_all input (0,0) in
  close_in input;
  dubs * trips

let () = Printf.printf "%d\n" res
