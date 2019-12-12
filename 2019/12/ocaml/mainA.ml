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

let step_gravitate () =
  for i = 0 to Array.length moons - 1 do
    let moon = moons.(i) in
    for j = i + 1 to Array.length moons - 1 do
      let othermoon = moons.(j) in
      for coord = 0 to Array.length moon - 1 do
        speeds.(i).(coord) <- speeds.(i).(coord) + sign (othermoon.(coord) - moon.(coord));
        speeds.(j).(coord) <- speeds.(j).(coord) + sign (moon.(coord) - othermoon.(coord))
      done
    done
  done

let step_move () =
  Array.iter2 (fun moon speed ->
      for coord = 0 to Array.length moon - 1 do
        moon.(coord) <- moon.(coord) + speed.(coord)
      done)
    moons speeds

let step () = step_gravitate (); step_move ();
  if debugging then begin print_string "STEP\n"; Array.iter (prpoint stdout) moons end

let energy_point v = Array.fold_left (fun acc x -> acc + abs x) 0 v

let energy () =
  let acc = ref 0 in
  Array.iter2 (fun  moon speed ->
      acc := !acc + (energy_point moon * energy_point speed))
    moons speeds;
  !acc

let () = for _i = 1 to 1000 do step () done

let energy = energy()

let () = Printf.printf "%d\n" energy
