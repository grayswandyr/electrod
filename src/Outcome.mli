open Containers

(** Represents a result trace (or the absence thereof).  *)

(** A valuation maps set/relation names to the tuples they contain. *)
type valuation
  
(** A state is either a plain state, or the target of a lasso from the last
    state of the trace. *)
type state
  
(** Nonempty, ordered sequence of states.  *)
type states = state list

(** An outcome represents the result of an analysis. It is either [None],
    meaning there is no resulting trace, or it is [Some _] in wihch case it
    carries a nonempty, ordered sequence of states, with at least one being the
    target of a loop ("lasso" step). *)
type t = private {
  trace : states option;
  nbvars : int;                 (** number of Booleans used  *)
  conversion_time : Mtime.span;
  analysis_time : Mtime.span
}

(** Represents the absence of trace (so usually: UNSAT). *)
val no_trace : int -> Mtime.span -> Mtime.span -> t

(** The list must be nonempty and must contain at least one lasso target. *)
val trace : int -> Mtime.span -> Mtime.span -> state list -> t

val valuation : (Name.t, TupleSet.t) List.Assoc.t -> valuation

val plain_state : valuation -> state
  
val loop_state : valuation -> state

(** Converts any state to a loop state *)
val to_loop : state -> state

val pp : format:[`XML | `Plain] -> Format.formatter -> t -> unit
