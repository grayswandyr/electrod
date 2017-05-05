(** AST for {i well-formed} Electrod problems. *)

open Containers


(** variables introduced by a binder *)
type var = BVar of Var.t
val bound_var : Var.t -> var

val equal_var : var -> var -> bool

(** any identifier: a binder-introduced variable or a set/relation
    name or tuple (the latter cannot appear out of parsing but will,
    after a substitution, when compiling to LTL formulas). *)
type ident = 
  | Var of Var.t
  | Name of Name.t
  | Tuple of Tuple.t

val var_ident : Var.t -> ident
val name_ident : Name.t -> ident
val tuple_ident : Tuple.t -> ident

val var_ident_of_bound_var : var -> ident

val equal_ident : ident -> ident -> bool 

type goal = (var, ident) GenGoal.t

type t = {
  file : string option;         (** name of the analyzed Electrod file *)
  domain : Domain.t;            (** always includes UNIV  *)
  instance : Instance.t;        (** may be empty  *)
  goals : goal list;            (** nonempty  *)
}

val make : string option -> Domain.t -> Instance.t -> goal list -> t


val must : Domain.t -> (var, ident) GenGoal.exp -> TupleSet.t
val may : Domain.t -> (var, ident) GenGoal.exp -> TupleSet.t
val sup : Domain.t -> (var, ident) GenGoal.exp -> TupleSet.t

val pp_var : Format.formatter -> var -> unit
val pp_ident : Format.formatter -> ident -> unit

include Intf.Print.S with type t := t
