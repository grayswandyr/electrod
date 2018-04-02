(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers 

module HC = Hashcons

(** De Bruijn-style formulas and expressions *)
(* TODO: De Bruijn indices start at 0 or 1? *)

type ('fml, 'exp, 'iexp) ot =
  | Run of ('fml, 'exp, 'iexp) ofml list [@@unboxed]

and ('fml, 'exp, 'iexp) ofml =
    | True 
  | False
  | Qual of rqualify * 'exp
  | RComp of 'exp * comp_op * 'exp
  | IComp of 'iexp * icomp_op * 'iexp
  | LUn of lunop * 'fml
  | LBin of 'fml * lbinop * 'fml
  | Quant of quant
             * 'exp osim_binding 
             * 'fml oblock
  | FIte of 'fml * 'fml * 'fml 
  | Block of 'fml oblock     

and 'fml oblock = 'fml list

(* int : number of bound variables (>= 1); if = 1 then disj must be false *)
and 'exp osim_binding = disj * int * 'exp

and disj = (bool [@opaque]) 

and quant = 
    | All 
  | Some_ 
  | No 
  | One 
  | Lone 

and lbinop = 
    | And 
  | Or 
  | Imp 
  | Iff 
  | U
  | R                           (* releases *)
  | S                           (* since *)

and lunop = 
    | F
  | G
  | Not 
  | O                           (* once *)
  | X
  | H
  | P                           (* previous *)

and comp_op = 
    | In 
  | NotIn 
  | REq 
  | RNEq

and icomp_op =
    | IEq
  | INEq
  | Lt 
  | Lte 
  | Gt 
  | Gte 

and ('fml, 'exp, 'iexp) oexp = {
      prim_exp : ('fml, 'exp, 'iexp) prim_oexp;
      arity : (int [@opaque]);   (* 0 = "polymorphic" arity (that of none) *)
    }

and ('fml, 'exp, 'iexp) prim_oexp =
    | None_ 
  | Univ 
  | Iden 
  | Var of int
  | Name of (Name.t [@opaque])
  | RUn of runop * 'exp
  | RBin of 'exp * rbinop * 'exp
  | RIte of 'fml * 'exp * 'exp
  | BoxJoin of 'exp * 'exp list (** <> []  *)
  | Compr of 'exp osim_binding * 'fml oblock
  | Prime of 'exp

and rqualify = 
    | ROne 
  | RLone 
  | RSome 
  | RNo 

and runop = 
    | Transpose 
  | TClos 
  | RTClos 

and rbinop = 
    | Union 
  | Inter 
  | Over 
  | LProj 
  | RProj 
  | Prod 
  | Diff 
  | Join

and ('fml, 'exp, 'iexp) oiexp =
    | Num of (int [@opaque])
  | Card of 'exp
  | IUn of iunop * 'iexp
  | IBin of 'iexp * ibinop * 'iexp

and iunop =
    | Neg

and ibinop =
    | Add
  | Sub
[@@deriving visitors { variety = "map"; name = "omap"},
            visitors { variety = "fold"; name = "ofold";
                       ancestors = ["VisitorsRuntime.map"] } 
]


type t =  (fml, exp, iexp) ot

and fml = Fml of (fml, exp, iexp) ofml HC.hash_consed [@@unboxed]

and sim_binding = exp osim_binding

and block = fml oblock

and prim_exp = (fml, exp, iexp) prim_oexp

and exp = Exp of (fml, exp, iexp) oexp HC.hash_consed [@@unboxed]

and iexp = Iexp of (fml, exp, iexp) oiexp HC.hash_consed [@@unboxed]

let fml_table : (fml, exp, iexp) ofml HC.t =
  HC.create 793

let exp_table : (fml, exp, iexp) oexp HC.t =
  HC.create 793

let iexp_table : (fml, exp, iexp) oiexp HC.t =
  HC.create 197

let hfml f : fml = 
  Fml (HC.hashcons fml_table f)

let exp ~ar (prim_exp : prim_exp) =
  { prim_exp; arity = ar; }

let hexp oe : exp =
  Exp (HC.hashcons exp_table oe)

let hiexp oie : iexp = 
  Iexp (HC.hashcons iexp_table oie)


class ['self] map = object (self : 'self)
  inherit [_] omap
  (* TODO is it correct to map the arity to itself?
     BTW the arity is specified as opaque *)
  method visit_'exp env (Exp { node; _}) =
    hexp (self # visit_oexp env node)

  method visit_'fml env (Fml { node; _}) =
    hfml (self # visit_ofml env node)

  method visit_'iexp env (Iexp { node; _}) =
    hiexp (self # visit_oiexp env node)
end

class virtual ['self] fold = object (self : 'self)
  inherit [_] ofold
  (* TODO is it correct to map the arity to itself?
     BTW the arity is specified as opaque *)
  method visit_'exp env (Exp { node; _}) =
    hexp (self # visit_oexp env node)

  method visit_'fml env (Fml { node; _}) =
    hfml (self # visit_ofml env node)

  method visit_'iexp env (Iexp { node; _}) =
    hiexp (self # visit_oiexp env node)
end

let run fs = Run fs

let true_ = hfml @@ True

let false_ = hfml @@ False

let qual qual e = hfml @@ Qual (qual, e)

let rcomp exp1 rcomp exp2 = hfml @@ RComp (exp1, rcomp, exp2)

let icomp exp1 rcomp exp2 = hfml @@ IComp (exp1, rcomp, exp2)

let lbinary fml1 binop fml2 = hfml @@ LBin (fml1, binop, fml2)

let sim_binding disj nbvars range : sim_binding =
  assert (nbvars <> 0);
  assert (not disj || nbvars > 1);
  (disj, nbvars, range)

let quant quant (decl : sim_binding) (block : block) = 
  assert (not List.(is_empty block));
  hfml @@ Quant (quant, decl, block)

let lunary lunop f = hfml @@ LUn (lunop, f)

let block block = hfml @@ Block block

let fite f t e = hfml @@ FIte (f, t, e)

(* let let_ decls block = Let (decls, block) *)

let all = All

let some = Some_

let no_ = No

let lone = Lone

let one = One

let and_ = And

let or_ = Or

let impl = Imp

let iff = Iff

let until = U

let releases = R

let since = S

let not_ = Not

let sometime = F

let always = G

let once = O

let next = X

let historically = H

let previous = P

let none = hexp @@ exp ~ar:0 @@ None_

let univ = hexp @@ exp ~ar:1 @@ Univ

let iden = hexp @@ exp ~ar:2 @@ Iden

let var ~ar n = hexp @@ exp ~ar @@ Var n

let name ~ar x = hexp @@ exp ~ar @@ Name x

let runary ~ar runop e = hexp @@ exp ~ar @@ RUn (runop, e)

let rbinary ~ar exp1 rbinop exp2 = hexp @@ exp ~ar @@ RBin (exp1, rbinop, exp2)

let rite ~ar cdt then_ else_ = hexp @@ exp ~ar @@ RIte (cdt, then_, else_)

(* TODO get rid of box join (here)? *)
let boxjoin ~ar caller callee = hexp @@ exp ~ar @@ BoxJoin (caller, callee)

let compr ~ar (decl : sim_binding) block =
  assert (not (List.is_empty block));
  hexp @@ exp ~ar @@ Compr (decl, block)

let prime ~ar e = hexp @@ exp ~ar @@ Prime e

let in_ = In

let not_in = NotIn

let req = REq

let rneq = RNEq

let ieq = IEq

let ineq = INEq

let lt = Lt

let lte = Lte

let gt = Gt

let gte = Gte

let rone = ROne

let rsome = RSome

let rlone = RLone

let rno = RNo

let transpose = Transpose

let tclos = TClos

let rtclos = RTClos

let union = Union

let inter = Inter

let over = Over

let lproj = LProj

let rproj = RProj

let prod = Prod

let diff = Diff

let join = Join

let num n = hiexp @@ Num n

let card e = hiexp @@ Card e

let iunary op e = hiexp @@ IUn (op, e)

let ibinary exp1 op exp2 = hiexp @@ IBin (exp1, op, exp2)

let neg = Neg

let add = Add

let sub = Sub

(* TESTS

   let e1 = 
   runary ~ar:2 transpose 
   @@ rbinary ~ar:2 
   (name ~ar:2 @@ Name.name "r") 
   inter 
   (runary ~ar:2 tclos @@ name ~ar:2 @@ Name.name "s")

   let e2 = 
   runary ~ar:2 transpose 
   @@ rbinary ~ar:2 
   (name ~ar:2 @@ Name.name "r") 
   inter 
   (runary ~ar:2 tclos @@ name ~ar:2 @@ Name.name "s")

   let _ = e1 == e2

*)


(******************************************************************************
 *  Pretty-printing TODO: handle De Bruijn indices
 *****************************************************************************)

let kwd_styled pf = Fmtc.(styled `Bold) pf

let pp_rqualify out x =
  Fmtc.(kwd_styled pf) out
  @@ match x with
  | ROne -> "one"
  | RLone -> "lone"
  | RSome -> "some"
  | RNo -> "no"

let pp_comp_op out =
  let open Fmtc in
  function
    | In -> (kwd_styled pf) out "in"
    | NotIn -> (kwd_styled pf) out "not in"
    | REq -> pf out "="
    | RNEq -> pf out "!="

let pp_icomp_op out = 
  let open Fmtc in
  function
    | Lt -> pf out "<"
    | IEq -> pf out "="
    | INEq -> pf out "!="
    | Lte -> pf out "<="
    | Gt -> pf out ">"
    | Gte -> pf out ">="

let pp_lunop out x =
  Fmtc.(kwd_styled pf) out
  @@ match x with
  | Not -> "not"
  | F -> "eventually"
  | G -> "always"
  | O -> "once"
  | X -> "next"
  | H -> "historically"
  | P -> "previous"

let pp_lbinop out x =
  Fmtc.(kwd_styled pf) out
  @@ match x with
  | And -> "and"
  | Or -> "or"
  | Imp -> "implies"
  | Iff -> "iff"
  | U -> "until"
  | R -> "releases"
  | S -> "since"

let pp_quant out x =
  Fmtc.(kwd_styled pf) out
  @@ match x with
  | Lone -> "lone"
  | One -> "one"
  | All -> "all"
  | Some_ -> "some"
  | No -> "no"

let pp_var out v =
  Fmtc.pf out "x/%d" v

let pp_binding ~sep pp_exp out (v, e) =
  let open Fmtc in
  pf out "%a@ %a@ %a"
    pp_var v
    sep ()
    pp_exp e

let pp_sim_binding pp_exp out (disj, vars, e) =
  let open Fmtc in
  pf out "%a%a :@ %a"
    (if disj then kwd_styled string else nop) "disj "
    (list ~sep:comma pp_var) (Sequence.to_list Int.Infix.(0 --^ vars))
    pp_exp e

let pp pp_fml out (Run fml) =
  let open Fmtc in
  begin
    (kwd_styled pf) out "run@ ";
    pf out "  %a" (box @@ list @@ pp_fml) fml
  end

let pp_block pp_fml out fmls = 
  let open Fmtc in
  pf out "@[<b 0>{@[<hv>%a@]@,}@]"
    (list ~sep:(sp **> semi) @@ pp_fml) fmls

let pp_ofml pp_fml pp_exp pp_iexp out = 
  let open Fmtc in
  function
    | True ->
        (kwd_styled pf) out "true"
    | False ->
        (kwd_styled pf) out "false"
    | Qual (q, e) ->
        pf out "@[<2>(%a@ %a)@]" pp_rqualify q pp_exp e
    | RComp (e1, op, e2) ->
        pf out "@[<2>(%a@ %a@ %a)@]"
          pp_exp e1
          pp_comp_op op
          pp_exp e2
    | IComp (e1, op, e2) ->
        pf out "@[<2>(%a@ %a@ %a)@]"
          pp_iexp e1
          pp_icomp_op op
          pp_iexp e2
    | LUn (op, fml) ->
        pf out "@[<2>(%a@ %a)@]" pp_lunop op pp_fml fml
    | LBin (f1, op, f2) ->
        pf out "@[<2>(%a@ %a@ %a)@]"
          pp_fml f1
          pp_lbinop op
          pp_fml f2        
    | Quant (q, decl, blk) ->
        pf out "@[<2>(%a %a@ %a)@]"
          pp_quant q
          (pp_sim_binding pp_exp) decl
          (pp_block pp_fml) blk       
    (* | Let (bindings, blk) ->
       pf out "%a %a@ %a"
       (kwd_styled string) "let"
       (list ~sep:(sp **> comma) @@ pp_binding ~sep:equal) bindings
       (pp_block pp_fml) blk *)
    | FIte (c, t, e) ->
        (* pf out "@[<hv2>(%a@ @[implies %a@]@ @[else %a@])@]" *)
        pf out "@[<hv>(%a) %a@;<1 2>@[(%a@])@;%a@;<1 2>@[(%a@])@]"
          pp_fml c
          (kwd_styled string) "implies"
          pp_fml t
          (kwd_styled string) "else"
          pp_fml e
    | Block fmls ->
        pp_block pp_fml out fmls

let pp_runop out = 
  let open Fmtc in
  function
    | Transpose -> pf out "~"
    | TClos -> pf out "^"
    | RTClos -> pf out "*"

let pp_rbinop out = 
  let open Fmtc in
  function
    | Union -> pf out "+"
    | Inter -> pf out "&"
    | Over -> pf out "++"
    | LProj -> pf out "<:"
    | RProj -> pf out ":>"
    | Prod -> pf out "->"
    | Diff -> pf out "-"
    | Join -> pf out "-"

let pp_prim_exp pp_fml pp_exp out = 
  let open Fmtc in
  function
    | None_ ->
        (styled Name.style pf) out "none"
    | Univ ->
        (styled Name.style pf) out "univ"
    | Iden ->
        (styled Name.style pf) out "iden"
    | Name id ->
        pf out "%a" Name.pp id
    | Var v -> 
        pp_var out v
    | RUn (op, e) ->
        pf out "@[<2>(%a%a)@]"
          pp_runop op
          pp_exp e
    | RBin (e1, Join, e2) ->    (* special one for join *)
        pf out "@[<2>(%a.%a)@]"
          pp_exp e1
          pp_exp e2
    | RBin (e1, op, e2) ->
        pf out "@[<2>(%a@ %a@ %a)@]"
          pp_exp e1
          pp_rbinop op
          pp_exp e2
    | RIte (c, t, e) ->
        pf out "@[<hv>%a %a@;<1 2>@[%a@]@;%a@;<1 2>@[%a@]@]"
          pp_fml c
          (kwd_styled string) "implies"
          pp_exp t
          (kwd_styled string) "else"
          pp_exp e
    | BoxJoin (e, args) ->
        pf out "@[<2>(%a%a)@]"
          pp_exp e
          (brackets @@ list ~sep:(sp **> comma) @@ pp_exp) args          
    | Compr (sim_binding, blk) ->
        pf out "%a"
          (braces_ @@
           pair ~sep:sp
             (pp_sim_binding pp_exp) 
             (pp_block pp_fml))
          (sim_binding, blk)
    | Prime e ->
        pf out "%a'" pp_exp e

let pp_exp pp_fml pp_exp out (Exp { node = e; _ }) =
  pp_prim_exp pp_fml pp_exp out e.prim_oexp

let pp_iunop out = 
  let open Fmtc in
  function
    | Neg -> pf out "-"

let pp_ibinop out = 
  let open Fmtc in
  function
    | Add -> pf out "+"
    | Sub -> pf out "-"

let pp_iexp pp_exp pp_iexp out = 
  let open Fmtc in
  function
    | Num n ->
        pf out "%d" n
    | Card e ->
        pf out "@[<2>(# %a)@]" pp_exp e
    | IUn (op, iexp) ->
        pf out "@[<2>(%a%a)@]" pp_iunop op
          pp_iexp iexp
    | IBin (e1, op, e2) -> 
        pf out "@[<2>(%a@ %a@ %a)@]"
          pp_iexp e1
          pp_ibinop op
          pp_iexp e2

