(** Type of relations. *)

(** A relation is either static (const) or dynamic (var). In the latter case, it
    may either be specified in a unique scope or with a scope for the initial
    instant and then a scope for all other instants. The arity is compulsorily
    specified by the user for empty bounds and optionally otherwise.*)
type t =
  | Const of { name : Name.t; arity : int; scope : Scope.t }
  | Var of { name : Name.t; arity : int; scope : Scope.t; fby : Scope.t option }


(** {1 Constructors} *)
val const : Name.t -> int -> Scope.t -> t
val var : Name.t -> int -> Scope.t -> Scope.t option -> t

  
(** Arity of the relation. (> 0) *)
val arity : t -> int

(** Tells whether the relation is a set or a relation of arity > 1. *)
val is_set : t -> bool
  
val is_nary : t -> bool

val is_const : t -> bool

val is_var : t -> bool

(** Returns the scope of a relation (for variable relations: not [fby]!)  *)
val scope : t -> Scope.t

val must : t -> TupleSet.t
val may : t -> TupleSet.t
val sup : t -> TupleSet.t

val pp : ?print_name:bool -> Format.formatter -> t -> unit

val to_string : ?print_name:bool -> t -> string
