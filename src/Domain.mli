(** The domain represents the set of relation declarations. *)

(** Virtually: a map between relation names and their definition as sets of
    tuples. *)
type t 

(** Constructor. *)
val empty : t

(** Adds an asociation to the domain. 
    The name must not be in the domain already. *)
val add : Name.t -> Relation.t -> t -> t

(** Checks whether a name is already bound in the map. *)
val mem : Name.t -> t -> bool

(** {1 Accessors}*)

(** May raise Not_found.  *)
val get_exn : Name.t -> t -> Relation.t

(** May rather return None. *)
val get : Name.t -> t -> Relation.t option

(** Returns the set of atoms in univ, represented as a {!type:Bound.t} *)
val univ_atoms : t -> Bound.t

(** Returns the map as an association list *)
val to_list : t -> (Name.t * Relation.t) list

include Intf.Print.S with type t := t

