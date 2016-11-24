(** Simplifies Elo problem by:
    - substitute let's
    - exploding quantifiers over many variables in simpler
      quantifiers (except when disj = true)
    - converting box join into classical join
 *)

(** Returns a simplified Elo problem *)
val transfo : (Elo.t, Elo.t) Transfo.t
