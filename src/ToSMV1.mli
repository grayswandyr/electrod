(** Implements a tranformation from {b simplified} Elo models to SMV
    models. Currently: only works on bare formulas.  *)

(** An implementation of printable LTL formulas in SMV.  *)
module Logic : LTL.PrintableLTL

val transfo : (Elo.t, Logic.t) Transfo.t
