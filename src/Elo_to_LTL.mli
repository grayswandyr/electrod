(** Provides a generic transformation from Elo formulas to {!LTL.S} ones.  *)

module MakeLtlConverter :
  functor (Ltl : LTL.S) ->
    sig
      val convert : Elo.t -> (Elo.var, Elo.ident) GenGoal.t -> Ltl.t
    end
