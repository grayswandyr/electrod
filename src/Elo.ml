
open Containers


(* variables introduced by a binder *)
type var = [ `Var of Var.t ] [@@deriving show]

(* any identifier: a binder-introduced variable or a set/relation name *)
type ident = [ var | `Name of Name.t ] [@@deriving show]

type goal = (var, ident) Raw_goal.t [@@deriving show]
              
type t = {
  file : string option;
  (* table of relations indexed by names (remark: a {!Relation.t} also knows its name) *)
  domain : Domain.t;
  goals : goal list;            (** nonempty  *)
}[@@deriving show]

let make file domain goals =
  { file; domain; goals }



module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
