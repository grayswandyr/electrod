
open Containers


(* variables introduced by a binder *)
type var = [ `Var of Var.t ] 

(* any identifier: a binder-introduced variable or a set/relation name *)
type ident = [ var | `Name of Name.t ]

type goal = (var, ident) GenGoal.t
              
type t = {
  file : string option;
  (* table of relations indexed by names (remark: a {!Relation.t} also knows its name) *)
  domain : Domain.t;
  goals : goal list;            (** nonempty  *)
}

let make file domain goals =
  { file; domain; goals }

let pp_var out (`Var v) =
  Var.pp out v

let pp_ident out = function
  | `Name n -> Name.pp out n
  | `Var v as var -> pp_var out var

let pp out { file; domain; goals } =
  let open Fmtc in
  pf out "%a@\n%a"
    Domain.pp domain
    (vbox @@ list @@ GenGoal.pp pp_var pp_ident) goals


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
