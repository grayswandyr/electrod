
open Containers


(* variables introduced by a binder *)
type var = BVar of Var.t

let bound_var v = BVar v

let equal_var id1 id2 = match id1, id2 with 
  | BVar v1, BVar v2 -> Var.equal v1 v2

(* any identifier: a binder-introduced variable or a set/relation name *)
type ident =
  | Var of Var.t
  | Name of Name.t
  | Tuple of Tuple.t

let var_ident i = Var i
let name_ident i = Name i
let tuple_ident i = Tuple i

let var_ident_of_bound_var (BVar v) = Var v

let equal_ident id1 id2 = match id1, id2 with
  | Tuple at1, Tuple at2 -> Tuple.equal at1 at2
  | Name n1, Name n2 -> Name.equal n1 n2
  | Var v1, Var v2 -> Var.equal v1 v2
  | (Name _, _)
  | (Var _, _)
  | (Tuple _, _)-> false

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

let pp_var out (BVar v) =
  Var.pp out v

let pp_ident out = function
  | Name n -> Fmtc.(styled `Cyan Name.pp) out n
  | Var v -> Fmtc.(styled `Yellow pp_var) out (BVar v)
  | Tuple at -> Fmtc.(styled `Cyan Tuple.pp) out at

let pp out { file; domain; instance; goals } =
  let open Fmtc in
  pf out "%a@\n%a@\n%a"
    Domain.pp domain
    Instance.pp instance
    (vbox @@ list @@ GenGoal.pp pp_var pp_ident) goals


let (must : Domain.t -> (var, ident) GenGoal.exp -> TupleSet.t),
    (sup : Domain.t -> (var, ident) GenGoal.exp -> TupleSet.t)  =
  let walk bound = object (self : 'self)
    inherit [_] GenGoal.fold as super

    method build_exp domain bnd _ = bnd
    method build_None_ domain = TupleSet.empty
    method build_Iden domain = bound @@ Domain.get_exn Name.iden domain
    method build_Union domain = TupleSet.union
    method build_Univ domain = bound @@ Domain.get_exn Name.univ domain

    method build_Ident domain = function
      | Var _ -> assert false
      | Tuple tuple -> TupleSet.of_tuples [tuple]
      | Name name -> bound @@ Domain.get_exn name domain

    method build_RBin domain l op r = op l r
    method build_RIte domain = failwith "Elo.build_RIte TODO"
    method build_Compr domain = assert false
    method build_Diff domain = TupleSet.diff
    method build_Inter domain = TupleSet.inter
    method build_Join domain = TupleSet.join
    method build_LProj domain = assert false
    method build_Over domain = assert false
    method build_RProj domain = assert false
    method build_RUn domain op e = op e
    method build_RTClos domain = assert false
    method build_TClos domain = assert false
    method build_Transpose domain = TupleSet.transpose
    method build_BoxJoin domain left right = assert false (* SIMPLIFIED *)
    method build_Prime domain = Fun.id
    method build_Prod domain = TupleSet.product
    method visit_'i domain = Fun.id

    method visit_'v domain = assert false

    method build_Add domain = assert false
    method build_All domain = assert false
    method build_And domain = assert false
    method build_Block domain = assert false
    method build_Card domain = assert false
    method build_F domain = assert false
    method build_FIte domain = assert false
    method build_False domain = assert false
    method build_G domain = assert false
    method build_Gt domain = assert false
    method build_Gte domain = assert false
    method build_H domain = assert false
    method build_IBin domain = assert false
    method build_IComp domain = assert false
    method build_IEq domain = assert false
    method build_INEq domain = assert false
    method build_IUn domain = assert false
    method build_Iff domain = assert false
    method build_Imp domain = assert false
    method build_In domain = assert false
    method build_LBin domain = assert false
    method build_LUn domain = assert false
    method build_Let domain = assert false
    method build_Lone domain = assert false
    method build_Lt domain = assert false
    method build_Lte domain = assert false
    method build_Neg domain = assert false
    method build_No domain = assert false
    method build_Not domain = assert false
    method build_NotIn domain = assert false
    method build_Num domain = assert false
    method build_O domain = assert false
    method build_One domain = assert false
    method build_Or domain = assert false
    method build_P domain = assert false
    method build_QAEN domain = assert false
    method build_QLO domain = assert false
    method build_Qual domain = assert false
    method build_R domain = assert false
    method build_RComp domain = assert false
    method build_REq domain = assert false
    method build_RLone domain = assert false
    method build_RNEq domain = assert false
    method build_RNo domain = assert false
    method build_ROne domain = assert false
    method build_RSome domain = assert false
    method build_S domain = assert false
    method build_Sat domain = assert false
    method build_Some_ domain = assert false
    method build_Sub domain = assert false
    method build_True domain = assert false
    method build_U domain = assert false
    method build_X domain = assert false
    method build_fml domain = assert false
    method build_iexp domain = assert false
  end
  in
  let must domain exp = 
    (walk @@ Relation.must) # visit_exp domain exp
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug @@ *)
    (*       fun m -> m "Elo.must(%a) = %a" *)
    (*                  (GenGoal.pp_exp pp_var pp_ident) exp TupleSet.pp res) *)
  in
  let sup domain exp =
    (walk @@ Relation.sup)  # visit_exp domain exp
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug @@ *)
    (*       fun m -> m "Elo.sup(%a) = %a" *)
    (*                  (GenGoal.pp_exp pp_var pp_ident) exp TupleSet.pp res) *)
  in
  CCCache.(with_cache (unbounded 79) must),
  CCCache.(with_cache (unbounded 79) sup)
  

let may =
  let aux domain exp =
    TupleSet.diff (sup domain exp) (must domain exp)
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug @@ *)
    (*       fun m -> m "Elo.may(%a) = %a" *)
    (*                  (GenGoal.pp_exp pp_var pp_ident) exp TupleSet.pp res) *)
  in
  CCCache.(with_cache (unbounded 79) aux)


  
let substitute = object (self : 'self)
  inherit [_] GenGoal.map as super

  method visit_'v _ = Fun.id

  method visit_'i _ = Fun.id

  method visit_Ident
           (env : (Var.t, (var, ident) GenGoal.prim_exp) CCList.Assoc.t )
           (id : ident) =
    match id with
      | Var var when List.Assoc.mem ~eq:Var.equal var env ->
          List.Assoc.get_exn ~eq:Var.equal var env
      | Var _ | Name _ | Tuple _ -> GenGoal.ident id
end



module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
