(** Tuples of atoms. *)

type t (** invariant: atoms <> empty *)

(** Builds a tuple out of a non-empty list of atoms. *)
val of_list1 : Atom.t list -> t

(** Builds a 1-tuple out of an atom.  *)
val tuple1 : Atom.t -> t

(** Returns the arity of the tuple. *)
val arity : t -> int

(** [t1 @@@ t2] yields the concatenantion of [t1] followed by [t2]
    (useful to compute the flat product of bounds/tuple sets). *)
val ( @@@ ) : t * t -> t

val equal : t -> t -> bool

(** Transposes a pair. *)
val transpose : t -> t

(** i-th element of a tuple. *)
val ith : int -> t -> Atom.t

(** [is_in_join tuple t1 t2] says whether tuple is the concatenation
    of [t1] (minus the last column) and [t2] (minus the ifrst column).  *)
val is_in_join : t -> t -> t -> bool

(** Joins two tuples (getting rid of the last and first columns (resp.))  *)
val join : t -> t -> t

include Intf.Print.S with type t := t

module Set : CCSet.S with type elt = t

