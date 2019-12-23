open Utils

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let tiles = Array.init 1000 (fun _ -> Array.make 1000 [])

let read_one l =
  match
    Scanf.sscanf l "#%d @ %d,%d: %dx%d" (fun id x y width height ->
        (id, x, y, width, height))
  with
  | exception _ -> abort "bad line %s\n" l
  | id, x, y, width, height ->
    for j = y to y + height - 1 do
      for i = x to x + width - 1 do
        tiles.(j).(i) <- id :: tiles.(j).(i)
      done
    done

let () =
  let input = open_in "../input.txt" in
  let rec aux () =
    match input_line input with
    | exception _ -> ()
    | l ->
      read_one l;
      aux ()
  in
  aux ();
  close_in input

(* claims are from 1 to 1349 included *)
let allclaims =
  let rec build acc i =
    if i = 1350 then acc else build (IntSet.add i acc) (i + 1)
  in
  build IntSet.empty 1

let good =
  Array.fold_left
    (Array.fold_left (fun good here ->
         match here with
         | [] | [ _ ] -> good
         | bad ->
           List.fold_left (fun good bad -> IntSet.remove bad good) good bad))
    allclaims tiles

let good = IntSet.elements good

let () =
  match good with
  | [ x ] -> Printf.printf "%d\n" x
  | _ -> abort "bad number of good things %d\n" (List.length good)
