let debugging = false

let debug m = if debugging then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let rec prlist pr sep ch = function
  | [] -> ()
  | [x] -> pr ch x
  | x :: rest -> pr ch x; Printf.fprintf ch sep; prlist pr sep ch rest

type vec = { x : int; y : int }

let prvec ch a = Printf.fprintf ch "(%d,%d)" a.x a.y

let rec load_astra pos nastra astra ch =
  match input_char ch with
  | exception End_of_file -> astra, nastra
  | '\n' -> load_astra {x = 0; y = pos.y + 1} nastra astra ch
  | '.' -> load_astra {x=pos.x + 1; y = pos.y} nastra astra ch
  | '#' -> load_astra {x=pos.x + 1; y = pos.y} (nastra+1) (pos::astra) ch
  | c -> abort "unknown symbol %c\n" c

let astra, nastra =
  let input = open_in "input.txt" in
  let r = load_astra {x=0;y=0} 0 [] input in
  close_in input;
  r

let () = debug "%d asteroids\n" nastra

let vdiff a b = {x = a.x - b.x; y = a.y - b.y}

let sign n = compare n 0

let same_direction a b =
  sign a.x = sign b.x && sign a.y = sign b.y &&
  a.y * b.x = b.y * a.x

let mk x y = {x;y}

let () = debug "true=%b false=%b false=%b false=%b\n"
    (same_direction (mk 0 1) (mk 0 2))
    (same_direction (mk 0 (-1)) (mk 0 2))
    (same_direction (mk 0 1) (mk 1 2))
    (same_direction (mk 1 2) (mk 2 3))

let is_hid base seen other =
  List.exists
    (fun seen -> same_direction (vdiff seen base) (vdiff other base))
    seen

let rec best_count_hidden best hid seen base = function
  | [] -> hid, base
  | other :: rest ->
    if is_hid base seen other
    then if hid+1 = fst best then best
      else best_count_hidden best (hid+1) seen base rest
    else best_count_hidden best hid (other::seen) base rest

let rec find_best best = function
  | [] -> best
  | v :: rest ->
    let best = best_count_hidden best 0 [] v astra in
    find_best best rest

let besthid, best = find_best (nastra,{x=0;y=0}) astra

(* we count the number of astra hidden from the best point. -1 because
   we don't count the station. *)
let () = debug "best is %a with %d visible\n" prvec best (nastra - besthid - 1)

let veq a b = a.x = b.x && a.y = b.y

let astra_view =
  List.filter (fun aster -> not (veq best aster)) astra
  |> List.map (fun a -> a, vdiff a best)

let vnorm a = a.x * a.x + a.y * a.y

(* WARNING y coord is inverted! negative is UP *)
(* (0,-1) is in NE, (1,0) in SE, (0,1) in SW, (-1,0) in NW *)
type quadrant = NE | SE | SW | NW
let quadrant a =
  match sign a.x, sign a.y with
  | 0, -1 | 1, -1 -> NE
  | 1, 0 | 1, 1 -> SE
  | 0, 1 | -1, 1 -> SW
  | -1, 0 | -1, -1 -> NW
  | _ -> abort "unexpected vector"

let compare_directions a b =
    let qa = quadrant a and qb = quadrant b in
    let c = compare qa qb in
    if c <> 0 then c
    else
      let xy v = float_of_int v.x /. float_of_int v.y in
      let yx v = float_of_int v.y /. float_of_int v.x in
      match qa with
      | NE -> compare (xy b) (xy a)
      | SE -> compare (yx a) (yx b)
      | SW -> compare (xy b) (xy a)
      | NW -> compare (yx a) (yx b)

let testpos = [mk (-1) (-3); mk (-1) (-2); (* NW *)
               mk (-1) (2); mk (-1) (3); (* SW *)
               mk 1 (3); mk 1 (2); (* SE *)
               mk 1 (-2); mk 1 (-3)] (* NE *)

let testsort = List.stable_sort compare_directions testpos

let () = debug "[%a] =\n[%a]\n" (prlist prvec "; ") (List.rev testpos) (prlist prvec "; ") testsort

let () = abort_unless
    (try List.for_all2 veq (List.rev testpos) testsort with Invalid_argument _ -> false)
    "bad compare_directions"

let astra_view = astra_view |> List.sort (fun (_,a) (_,b) -> compare_directions a b)

let rec packetize acc cur = function
  | [] -> List.rev (cur :: acc)
  | x :: rest ->
    if same_direction (snd (List.hd cur)) (snd x)
    then packetize acc (x::cur) rest
    else packetize (cur::acc) [x] rest

(* each of the element lists contains astra in the same direction from
   [best] *)
let astra_view =
  match astra_view with
  | [] -> abort "no astra"
  | x :: rest -> packetize [] [x] rest

let compare_norm (_,a) (_,b) = compare (vnorm a) (vnorm b)

(* each of the element lists contains astra in the same direction from
   [best], ordered by increasing distance from [best] *)
let astra_view =
  astra_view
  |> List.map (List.sort compare_norm)

let () = debug "packetized:\n%a\ndone\n"
    (prlist (prlist (fun ch (a,_) -> prvec ch a) ";") "\n") astra_view

let rec find_nth_kill next_layers n = function
  | [] -> if next_layers = [] then abort "not enough astra!"
    else find_nth_kill [] n (List.rev next_layers)
  | [] :: _ -> abort "empty layer!"
  | (x :: layer) :: rest ->
    if n = 0 then x
    else find_nth_kill
        (if layer = [] then next_layers else layer :: next_layers)
        (n-1) rest

let kill,_ = find_nth_kill [] (200-1) astra_view

let () = Printf.printf "%a -> %d\n" prvec kill (kill.x * 100 + kill.y)
