open Containers
    
type t = state list 

(** A state is either a plain state, or the target of a lasso from the last
    state of the trace. *)
and state =
  | Plain of valuation
  | Loop of valuation

(** A valuation maps set/relation names to the tuples they contain. *)
and valuation = (Name.t, TupleSet.t) List.Assoc.t


let valuation valu =
  valu

let plain_state v = Plain v

let loop_state v = Loop v

let absent = []

let make states =
  assert (states <> []);
  states


open Fmtc
    
let pp_valuation =
  list ~sep:(const string ", ")
  @@ pair ~sep:equal Name.pp TupleSet.pp

let pp_state out = function
  | Plain v -> brackets pp_valuation out v 
  | Loop v -> (angles pp_valuation) out v

let pp =
  vbox @@ list pp_state
