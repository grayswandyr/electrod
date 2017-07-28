open Containers

(** Represents a result trace (or the absence thereof).  *)

(** A trace is a {b nonempty} list of states with at least one lasso target (the
    order is that of the list itself).  *)
type t

(** A state is either a plain state, or the target of a lasso from the last
    state of the trace. *)
type state

(** A valuation maps set/relation names to the tuples they contain. *)
type valuation
  

val valuation : (Name.t, TupleSet.t) List.Assoc.t -> valuation

val plain_state : valuation -> state
  
val loop_state : valuation -> state

(** Converts any state in a loop state *)
val to_loop : state -> state


(** The list must be nonempty and must contain at least one lasso target. *)
val make : state list -> t

val pp : format:[`XML | `Plain] -> Format.formatter -> t -> unit
