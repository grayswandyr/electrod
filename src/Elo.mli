(** AST for {i well-formed} Electrod problems. *)

open Containers


(* variables introduced by a binder *)
type var = [ `Var of Var.t ] [@@deriving show]

(* any identifier: a binder-introduced variable or a set/relation name *)
type ident = [ var | `Name of Name.t ] [@@deriving show]

type goal = (var, ident) Raw_goal.t [@@deriving show]

type t = {
  file : string option;         (** name of the analyzed Electrod file *)
  domain : Domain.t;            (** always includes UNIV  *)
  goals : goal list;            (** nonempty  *)
}[@@deriving show]

val make : string option -> Domain.t -> goal list -> t

include Intf.Print.S with type t := t
