open Utils

let tiles = Array.init 1000 (fun _ -> Array.make 1000 0)

let read_one l =
  match
    Scanf.sscanf l "#%d @ %d,%d: %dx%d" (fun _id x y width height ->
        (x, y, width, height))
  with
  | exception _ -> abort "bad line %s\n" l
  | x, y, width, height ->
    for j = y to y + height - 1 do
      for i = x to x + width - 1 do
        tiles.(j).(i) <- tiles.(j).(i) + 1
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

let count =
  Array.fold_left
    (Array.fold_left (fun count claims ->
         if claims >= 2 then count + 1 else count))
    0 tiles

let () = Printf.printf "%d\n" count
