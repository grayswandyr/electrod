(** Type of relations. *)

(** A relation is either static (const) or dynamic (var). In the latter case, it
    may either be specified in a unique scope or with a scope for the initial
    instant and then a scope for all other instants. *)
type t =
  | Const of { name : Name.t; scope : Scope.t }
  | Var of { name : Name.t; scope : Scope.t; fby : Scope.t option }


(** {1 Constructors} *)
val const : Name.t -> Scope.t -> t
val var : Name.t -> Scope.t -> Scope.t option -> t

  
(** Arity of the relation. *)
val arity : t -> int

(** Tells whether the relation is a set or a relation of arity > 1. *)
val is_set : t -> bool
  
val is_nary : t -> bool

val is_const : t -> bool

val is_var : t -> bool



val pp : ?print_name:bool -> Format.formatter -> t -> unit

val to_string : ?print_name:bool -> t -> string
