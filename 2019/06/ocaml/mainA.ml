let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

module Int = struct
  type t = int

  let compare : int -> int -> int = compare
end

module SMap = Map.Make (String)
module IMap = Map.Make (Int)
module ISet = Set.Make (Int)

module Point : sig
  type t

  val parse : string -> t

  val print : t -> string

  val compare : t -> t -> int

  val com : t

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t
end = struct
  include Int
  module Map = IMap
  module Set = ISet

  let points = ref SMap.empty

  let rpoints = ref IMap.empty

  let npoints = ref 0

  let parse s =
    let r = ref !npoints in
    points :=
      SMap.update s
        (function
          | None ->
            incr npoints;
            rpoints := IMap.add !r s !rpoints;
            Some !r
          | Some n as v ->
            r := n;
            v)
        !points;
    !r

  let com = parse "COM"

  let print p = IMap.find p !rpoints
end

module PMap = Point.Map
module PSet = Point.Set

let rec parse ch acc =
  match input_line ch with
  | exception End_of_file -> acc
  | l ->
    let acc =
      match String.split_on_char ')' l with
      | [ a; b ] ->
        let pa = Point.parse a in
        let pb = Point.parse b in
        PMap.add pb pa acc
      | _ -> abort "invalid line %s\n" l
    in
    parse ch acc

(* [Map.find p direct] gives the point [p] orbits around (Not_found for COM)  *)
let direct =
  let input = open_in "input.txt" in
  let r = parse input PMap.empty in
  close_in input;
  r

let rec make_counts visited counts p =
  match PMap.find_opt p counts with
  | Some count -> (count, counts)
  | None ->
    let visited =
      let v' = PSet.add p visited in
      if visited == v' then abort "Cycle detected through %s\n" (Point.print p);
      v'
    in
    (match PMap.find_opt p direct with
    | None -> (0, PMap.add p 0 counts)
    | Some p' ->
      let countp', counts = make_counts visited counts p' in
      let countp = countp' + 1 in
      (countp, PMap.add p countp counts))

let count, counts =
  PMap.fold
    (fun p _ (count, counts) ->
      let cp, counts = make_counts PSet.empty counts p in
      (count + cp, counts))
    direct (0, PMap.empty)

let () = Printf.printf "%d\n" count
