
open Containers


(* variables introduced by a binder *)
type var = [ `Var of Var.t ]

let equal_var id1 id2 = match id1, id2 with 
  | `Var v1, `Var v2 -> Var.equal v1 v2

(* any identifier: a binder-introduced variable or a set/relation name *)
type ident = [ var | `Name of Name.t | `Atom of Atom.t]

let equal_ident id1 id2 = match id1, id2 with
  | `Atom at1, `Atom at2 -> Atom.equal at1 at2
  | `Name n1, `Name n2 -> Name.equal n1 n2
  | `Var v1, `Var v2 -> Var.equal v1 v2
  | (`Name _, _)
  | (`Var _, _)
  | (`Atom _, _)-> false

type goal = (var, ident) GenGoal.t
              
type t = {
  file : string option;
  (* table of relations indexed by names (remark: a {!Relation.t} also knows its name) *)
  domain : Domain.t;
  instance : Instance.t;
  goals : goal list;            (** nonempty  *)
}

let make file domain instance goals =
  { file; domain; instance; goals }

let pp_var out (`Var v) =
  Var.pp out v

let pp_ident out = function
  | `Name n -> Fmtc.(styled `Cyan Name.pp) out n
  | `Var _ as var -> Fmtc.(styled `Yellow pp_var) out var
  | `Atom at -> Fmtc.(styled `Cyan Atom.pp) out at

let pp out { file; domain; instance; goals } =
  let open Fmtc in
  pf out "%a@\n%a@\n%a"
    Domain.pp domain
    Instance.pp instance
    (vbox @@ list @@ GenGoal.pp pp_var pp_ident) goals


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
