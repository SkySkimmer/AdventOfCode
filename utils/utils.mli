val debugging : int
(** debugging level *)

val debug : int -> ('a, out_channel, unit) format -> 'a
(** print message to stderr if debugging >= 1 *)

val abort : ('a, out_channel, unit, 'b) format4 -> 'a
(** print message to stderr then exit 1 *)

val abort_unless : bool -> ('a, out_channel, unit, unit) format4 -> 'a
(** abort unless the bool is [true] *)

val prlist :
  (Format.formatter -> 'a -> unit) ->
  (unit, Format.formatter, unit) format ->
  Format.formatter ->
  'a list ->
  unit

val input_lines : in_channel -> string list
