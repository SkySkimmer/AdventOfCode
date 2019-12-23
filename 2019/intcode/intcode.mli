
type program

val input_program : in_channel -> program
(** Reads 1 line an interprets it as an intcode program. *)

type state
(** Mutable intcode state. *)

val make_state : ?memsize:int -> program -> state
(** If given explicitly memsize should be at least the size of the
   program. *)

type status =
  | WaitInput of (int -> unit)
  | HaveOutput of int
  | Done

val run : state -> status
(** Run the program until an input or output instruction. The closure
   in [WaitInput] should be called exactly once before calling [run]
   again. *)

val debug : bool ref
(** Print debug info (what instructions get run, etc) if set to [true]. *)
