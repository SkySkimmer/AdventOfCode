let debugging =
  match Sys.argv with
  | [| _; "-debug" |] -> 1
  | [| _; "-debug"; s |] -> int_of_string s
  | _ -> 0

let debug lvl m =
  if debugging >= lvl then Printf.eprintf m else Printf.ifprintf stderr m

let abort m = Printf.kfprintf (fun _ -> exit 1) stderr m

let abort_unless b m = if not b then abort m else Printf.ifprintf stderr m

let rec prlist pr sep ch = function
  | [] -> ()
  | [ x ] -> pr ch x
  | x :: rest ->
    pr ch x;
    Format.fprintf ch sep;
    prlist pr sep ch rest

let rec input_lines_aux ch acc =
  match input_line ch with
  | exception _ -> acc
  | l -> input_lines_aux ch (l :: acc)

let input_lines ch = List.rev (input_lines_aux ch [])

module Int = struct
  type t = int

  external compare : int -> int -> int = "%compare"
end

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
