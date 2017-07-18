open Containers

(** Represents a result trace (or the absence thereof).  *)

(** A trace is a {b nonempty} list of states (so the order is that of the list
    itself).  *)
type t

(** A state is either a plain state, or the target of a lasso from the last
    state of the trace. *)
type state

(** A valuation maps set/relation names to the tuples they contain. *)
type valuation
  

val valuation : (Name.t, TupleSet.t) List.Assoc.t -> valuation

val plain_state : valuation -> state
  
val loop_state : valuation -> state

(** The list must be nonempty. *)
val make : state list -> t

val pp : Format.formatter -> t -> unit
