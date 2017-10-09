(** Tranforms raw ASTs into "massaged" ones (conforming to Elo). *)

(** Determines the whole domain of the problem. *)
val transfo : (Raw.raw_problem, Elo.t) Transfo.t

