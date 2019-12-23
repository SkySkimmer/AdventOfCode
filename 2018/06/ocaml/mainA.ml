open Utils.Util

let lines =
  let input = open_in "../input.txt" in
  let l = input_lines input in
  close_in input;
  l

let coords =
  List.map
    (fun line ->
      match String.split_on_char ',' line with
      | [ l; r ] ->
        (int_of_string (String.trim l), int_of_string (String.trim r))
      | _ -> abort "could not parse %S\n" line)
    lines

let maxx, maxy =
  List.fold_left
    (fun (maxx, maxxy) (x, y) -> (max maxx x, max maxxy y))
    (0, 0) coords

type owner =
  | Owned of (int * int) * int  (** owner * distance *)
  | Contested of int  (** distance *)
  | Unknown

let tiles = Array.make_matrix (maxy + 1) (maxx + 1) Unknown

let dist (x, y) (x', y') = abs (x - x') + abs (y - y')

let rec find_owner xy cur = function
  | [] -> cur
  | c :: rest ->
    let d = dist xy c in
    let cur =
      match cur with
      | Contested d' -> if d < d' then Owned (c, d) else cur
      | Owned (_, d') ->
        if d < d' then Owned (c, d) else if d' < d then cur else Contested d
      | Unknown -> Owned (c, d)
    in
    find_owner xy cur rest

let () =
  for i = 0 to maxy do
    let tiles = tiles.(i) in
    for j = 0 to maxx do
      tiles.(j) <- find_owner (j, i) Unknown coords
    done
  done

module PMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

type area = Finite of int | Infinite

let counts = ref PMap.empty

let () =
  for i = 0 to maxy do
    for j = 0 to maxx do
      match tiles.(i).(j) with
      | Unknown -> abort "unknown!\n"
      | Contested _ -> ()
      | Owned (owner, _) ->
        if i = 0 || i = maxy || j = 0 || j = maxx then
          counts := PMap.add owner Infinite !counts
        else
          counts :=
            PMap.update owner
              (function
                | None -> Some (Finite 1)
                | Some Infinite -> Some Infinite
                | Some (Finite n) -> Some (Finite (n + 1)))
              !counts
    done
  done

let counts = !counts

let r =
  PMap.fold
    (fun _ area best ->
      match area with Infinite -> best | Finite n -> max best n)
    counts 0

let () = Printf.printf "%d\n" r
