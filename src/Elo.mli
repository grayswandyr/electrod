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
  instance : Instance.t;        (** may be empty  *)
  goals : goal list;            (** nonempty  *)
}

val make : string option -> Domain.t -> Instance.t -> goal list -> t

val pp_var : Format.formatter -> var -> unit
val pp_ident : Format.formatter -> ident -> unit

include Intf.Print.S with type t := t
