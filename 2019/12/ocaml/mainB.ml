let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let moons =
  let input = Scanf.Scanning.open_in "input.txt" in
  let rec aux acc =
    match Scanf.bscanf input "<x=%d, y=%d, z=%d>\n" (fun x y z -> [|x;y;z|]) with
    | exception _ -> acc
    | v -> aux (v::acc)
  in
  let r = aux [] in
  Scanf.Scanning.close_in input;
  r

let moons = Array.of_list (List.rev moons)

let () = debug "%d moons\n" (Array.length moons)

let prpoint ch v =
  Printf.fprintf ch "<x=%d, y=%d, z=%d>\n" v.(0) v.(1) v.(2)

(* don't use array.make as it would create only 1 sub array! XD *)
let speeds = Array.init (Array.length moons) (fun _ -> [|0;0;0|])

let sign n = compare n 0

let step_gravitate coord =
  for i = 0 to Array.length moons - 1 do
    let moon = moons.(i) in
    for j = i + 1 to Array.length moons - 1 do
      let othermoon = moons.(j) in
      speeds.(i).(coord) <- speeds.(i).(coord) + sign (othermoon.(coord) - moon.(coord));
      speeds.(j).(coord) <- speeds.(j).(coord) + sign (moon.(coord) - othermoon.(coord))
    done
  done

let step_move coord =
  Array.iter2 (fun moon speed -> moon.(coord) <- moon.(coord) + speed.(coord))
    moons speeds

let step coord = step_gravitate coord; step_move coord

module PMap = Map.Make(struct type t = (int * int) list let compare = compare end)

let get_state coord =
  let acc = ref [] in
  for i = 0 to Array.length moons - 1 do
    acc := (moons.(i).(coord), speeds.(i).(coord)) :: !acc
  done ;
  !acc

let rec find_repeat coord seen i =
  let cur = get_state coord in
  let previous = ref None in
  let seen = PMap.update cur (function
      | None -> Some i
      | Some _ as r -> previous := r; r)
      seen
  in
  match !previous with
  | Some j -> j, i
  | None -> step coord; find_repeat coord seen (i+1)

let repeats = Array.init (Array.length moons.(0))
    (fun coord ->
       let r0, r = find_repeat coord PMap.empty 0 in
       abort_unless (r0 = 0) "unexpected repeating state is not first state\n";
       r)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let lcm a b = if a = 0 then b (* <- we use 0 as the start value for the fold *)
  else let g = gcd a b in (a * b) / g

let lcm = Array.fold_left lcm 0 repeats

let () = Printf.printf "%d\n" lcm
