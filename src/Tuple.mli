(** Tuples of atoms. *)

type t (** invariant: atoms <> empty *)

(** Builds a tuple out of a non-empty list of atoms. *)
val of_list1 : Atom.t list -> t

(** Builds a 1-tuple out of an atom.  *)
val tuple1 : Atom.t -> t

(** Returns the arity of the tuple. *)
val arity : t -> int

(** [append t1 t2] yields the concatenantion of [t1] followed by [t2]
    (useful to compute the flat product of bounds/tuple sets). *)
val append : t * t -> t

val equal : t -> t -> bool

include Intf.Print.S with type t := t

module Set : CCSet.S with type elt = t

