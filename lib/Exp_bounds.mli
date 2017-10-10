(** Computation of bounds for Elo expressions. *)

type bounds = {
  must : TupleSet.t;
  sup : TupleSet.t;
  may : TupleSet.t;
}

(** Computes the must/may/sup bounds of an expression [exp], given the [domain]
    and a substitution [subst] (substituting a tuple for a variable) *)
val bounds : 
  (Var.t, Tuple.t) CCList.Assoc.t ->
  Domain.t ->
  (Elo.var, Elo.ident) GenGoal.exp -> bounds
