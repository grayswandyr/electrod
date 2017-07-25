open Containers

(** A trace is a list of states with at least one lasso target. *)
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

let to_loop = function Loop v | Plain v -> Loop v

let make states =
  assert (states <> []
          && List.exists (function Loop _ -> true | Plain _ -> false) states);
  states


open Fmtc
    
let pp_valuation out valu =
  pf out "%a"
    (hvbox @@ list ~sep:sp
     @@ pair ~sep:equal Name.pp TupleSet.pp)
  @@ List.sort (fun (n1, _) (n2, _) -> Name.compare n1 n2) valu

let pp_state out = function
  | Plain v -> (const string "  " **< brackets_ pp_valuation) out v 
  | Loop v -> (const string "->" **< brackets_ pp_valuation) out v

let pp out trace =
  assert (trace <> []);
  list ~sep:sp pp_state out trace
