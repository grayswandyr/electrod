(** Abstract type for a converter from Elo models to (abstract) LTL formulas.  *)

open Containers

module type S = sig
  type atomic                     (* LTL propositional atoms *)
  type ltl                      (* ltl formula *)

  val color :
    Elo.t ->
    (Elo.var, Elo.ident) GenGoal.fml ->
    Invar_computation.goal_color

  val convert :
    Elo.t ->
    (Elo.var, Elo.ident) GenGoal.fml ->
    atomic CCSet.sequence * atomic CCSet.sequence
    * string * ltl

end
