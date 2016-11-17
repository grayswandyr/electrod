(** AST for {i well-formed} Electrod problems. *)

open Containers


(** variables introduced by a binder *)
type var = [ `Var of Var.t ]

val equal_var : var -> var -> bool

(** any identifier: a binder-introduced variable or a set/relation name *)
type ident = [ var | `Name of Name.t ]

val equal_ident : ident -> ident -> bool 

type goal = (var, ident) GenGoal.t

type t = {
  file : string option;         (** name of the analyzed Electrod file *)
  domain : Domain.t;            (** always includes UNIV  *)
  goals : goal list;            (** nonempty  *)
}

val make : string option -> Domain.t -> goal list -> t

include Intf.Print.S with type t := t
