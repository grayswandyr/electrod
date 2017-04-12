(** Relation scopes. *)

type t = private
  | Exact of TupleSet.t             (** means: lower bound = upper bound *)
  | Inexact of TupleSet.t * TupleSet.t (** invariant: lower bound <> upper bound *)


(** {1 Constructors} *)
                              
val exact : TupleSet.t -> t
val inexact : TupleSet.t -> TupleSet.t -> t

(** 0 if the arity cannot be inferred (= is unknown), [n > 0] otherwise. *)
val inferred_arity : t -> int 

include Intf.Print.S with type t := t
