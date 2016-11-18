(** Relation scopes. *)


(** Remark: a lower bound may be the empty set, in which case its arity would be
    counted as 0: this value should be considered as "compatible" with any arity
    for the upper bound *)
type t = private
  | Exact of Bound.t             (** means: lower bound = upper bound *)
  | Inexact of Bound.t * Bound.t (** invariant: lower bound <> upper bound *)


(** {1 Constructors} *)
val exact : Bound.t -> t
val inexact : Bound.t -> Bound.t -> t

val arity : t -> int option

include Intf.Print.S with type t := t
