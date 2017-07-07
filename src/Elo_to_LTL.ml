open Containers

module type S = sig
  type atom                     (* LTL propositional atoms *)
  type ltl                      (* ltl formula *)

  val convert :
    Elo.t ->
    (Elo.var, Elo.ident) GenGoal.fml ->
    atom CCSet.sequence * atom CCSet.sequence * ltl
end
