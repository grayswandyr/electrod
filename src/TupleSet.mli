(** Type for sets of tuples. *)


(** Set of tuples. Invariant: all tuples in the tuple set have the same arity *)
type t

(** The empty tuple set. *)
val empty : t

(** Requires: [tuples] is a nonempty list for tuples of the same arity. *)
val of_tuples : Tuple.t list -> t

(** Arity of a tuple set. 0 if the set is empty, [n > 0] otherwise. *)
val inferred_arity : t -> int

(** Tells whether the tuple set denotes the empty set. *)
val is_empty : t -> bool

(** Tuples in a tuple set. *)
val tuples : t -> Tuple.Set.t
                 
(** Computes the union of two tuple sets [b1] and [b2]. 

    Requires: [b1] and [b2] have the same arity. *)
val union : t -> t -> t
  
(** Computes the intersecion of two tuple sets [b1] and [b2]. *)
val inter : t -> t -> t

(** [product b1 b2] computes the {b flat} product of [b1] and [b2].
    Recall the product is {i not} empty if any of [b1] or [b2] is. *)
val product : t -> t -> t

(** [subset b1 b2] returns [true] if [b1] is included in [b2].  *)
val subset : t -> t -> bool

(** [equal b1 b2] returns [true] if [b1] is equal [b2]. *)
val equal : t -> t -> bool

(** Compares tuple sets against the inclusion ordering *)
val compare : t -> t -> int

(** [mem t ts] tells whether [t] is in [ts]. *)
val mem : Tuple.t -> t -> bool

(** Cardinality of a tuple set  *)
val size : t -> int

(** Set difference.  *)
val diff : t -> t -> t
  
(** Transposition.  *)
val transpose : t -> t

(** Diagonal of a set.  *)
val diagonal : t -> t

(** Join of two tuple sets.  *)
val join : t -> t -> t

(** Infix version(s) of preivous operations.  *)
module Infix : sig
  (** [mem] *)
  val ( $: ) : Tuple.t -> t -> bool
end

val to_seq : t -> Tuple.t CCSet.sequence

include Intf.Print.S with type t := t
