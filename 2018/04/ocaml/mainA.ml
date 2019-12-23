open Utils.Util

let lines =
  let input = open_in "../input.txt" in
  let r = input_lines input in
  close_in input;
  r

let lines = List.sort compare lines

type entry_type = StartShift of int | Sleep | Wake

type entry = { day : string; hour : int; minute : int; what : entry_type }

let parse_line l =
  let fail () = abort "could not parse \"%s\"\n" l in
  match String.split_on_char ' ' l with
  | day :: time :: rest ->
    let day = String.sub day 1 (String.length day - 1) in
    let hour, minute =
      match String.split_on_char ':' time with
      | [ hour; minute ] ->
        ( int_of_string hour,
          int_of_string (String.sub minute 0 (String.length minute - 1)) )
      | _ -> fail ()
    in
    let what =
      match rest with
      | [ "Guard"; id; "begins"; "shift" ] ->
        StartShift (int_of_string (String.sub id 1 (String.length id - 1)))
      | [ "falls"; "asleep" ] -> Sleep
      | [ "wakes"; "up" ] -> Wake
      | _ -> fail ()
    in
    { day; hour; minute; what }
  | _ -> fail ()

let entries = List.map parse_line lines

let rec sleeps acc cur = function
  | [] -> List.rev acc
  | { what = StartShift cur; _ } :: rest -> sleeps acc cur rest
  | { what = Sleep; minute = sleep; _ }
    :: { what = Wake; minute = wake; _ } :: rest ->
    sleeps ((cur, sleep, wake) :: acc) cur rest
  | _ -> abort "unexpected what\n"

let sleeps = sleeps [] 0 entries

let sleep_counts =
  List.fold_left
    (fun counts (cur, sleep, wake) ->
      let len = wake - sleep in
      IntMap.update cur
        (function None -> Some len | Some len' -> Some (len + len'))
        counts)
    IntMap.empty sleeps

let best, _ =
  IntMap.fold
    (fun cur curcnt ((_, bestcnt) as best) ->
      if curcnt > bestcnt then (cur, curcnt) else best)
    sleep_counts (0, 0)

let mins = Array.make 60 0

let () =
  List.iter
    (fun (cur, sleep, wake) ->
      if cur = best then
        for min = sleep to wake - 1 do
          mins.(min) <- mins.(min) + 1
        done)
    sleeps

let bestmin, _ =
  Utils.CArray.fold_left_i
    (fun min ((_, bestcnt) as best) cnt ->
      if cnt > bestcnt then (min, cnt) else best)
    (0, 0) mins

let () = Printf.printf "%d\n" (best * bestmin)
