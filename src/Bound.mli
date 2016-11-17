(** Type for relation bounds. *)


(** Set of tuples. Invariant: all tuples in the bound have the same arity *)
type t = {
  tuples : Tuple.Set.t;
  arity : int option;           (** None = arity of empty set or 'none' *)
}

(** The empty bound. *)
val empty : t

(** Requires: [tuples] is a nonempty list for tuples of the same arity. *)
val of_tuples : Tuple.t list -> t

(** Arity of a bound. *)
val arity : t -> int option

(** Arity of the empty bound. Meant to be compatible with any positive arity *)
val empty_arity : int option

(** Tells whether the bound denotes the empty set. *)
val is_empty : t -> bool

(** Tuples in a bound. *)
val tuples : t -> Tuple.Set.t
                 
(** Computes the union of two bounds [b1] and [b2]. 

    Requires: [b1] and [b2] have the same arity. *)
val union : t -> t -> t
  
(** Computes the intersecion of two bounds [b1] and [b2]. 

    Requires: [b1] and [b2] have the same arity. *)
val inter : t -> t -> t

(** [product b1 b2] computes the {b flat} product of [b1] and [b2].
    Recall the product is {i not} empty if any of [b1] or [b2] is. *)
val product : t -> t -> t

(** [subset b1 b2] returns [true] if [b1] is included in [b2]. 

    Requires: [b1] and [b2] have the same arity (or b1 is empty). *)
val subset : t -> t -> bool

(** [equal b1 b2] returns [true] if [b1] is equal [b2]. 

    Requires: [b1] and [b2] have the same arity. *)
val equal : t -> t -> bool

(** Compares bounds against the inclusion ordering *)
val compare : t -> t -> int

(** [mem t ts] telles whether [t] is in [ts]. *)
val mem : Tuple.t -> t -> bool

(** Cardinality of a bound  *)
val size : t -> int

include Intf.Print.S with type t := t
