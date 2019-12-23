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

  val equal : t -> t -> bool

  val santa : t

  val you : t

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t
end = struct
  include Int
  module Map = IMap
  module Set = ISet

  let equal : int -> int -> bool = ( == )

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

  let santa = parse "SAN"

  let you = parse "YOU"

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

(* Map with links in both direction. eg if [p] orbits around [p'] we
   have [p -> p'] and [p' -> p] in the map.*)
let double =
  let addp p = function
    | None -> Some (PSet.singleton p)
    | Some m -> Some (PSet.add p m)
  in
  PMap.fold
    (fun p p' acc ->
      let acc = PMap.update p (addp p') acc in
      let acc = PMap.update p' (addp p) acc in
      acc)
    direct PMap.empty

let search target start =
  (* queue -> BFS. If we use a list (stack) instead we get DFS. *)
  let work = Queue.create () in
  Queue.add (start, 0) work;
  let seen = ref PSet.empty in
  let rec search () =
    match Queue.take work with
    | exception Queue.Empty -> abort "path not found"
    | p, dist ->
      if Point.equal target p then dist
      else if PSet.mem p !seen then search ()
      else begin
        seen := PSet.add p !seen;
        let neighbors = PMap.find p double in
        PSet.iter (fun p -> Queue.add (p, dist + 1) work) neighbors;
        search ()
      end
  in
  search ()

let dist = search (PMap.find Point.santa direct) (PMap.find Point.you direct)

let () = Printf.printf "%d\n" dist
