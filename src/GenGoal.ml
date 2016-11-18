
type 'a located = {
  data : 'a;
  loc : Location.t;
}

let pp_located pp_data out { data; _ } =
  Fmtc.pf out "%a" pp_data data

(* ['v] is the type of variables introduced in quantifiers, ['i] is the type of
   any identifier (a variable like in the former case or a relation name) *)
type ('v, 'i) t =
  | Sat of ('v, 'i) fml list

(** Formulas and expressions *)

and ('v, 'i) fml = ('v, 'i) prim_fml located

and ('v, 'i) prim_fml =
  | FBuiltin of string * ('v, 'i) exp list (** nonempty *)
  | True 
  | False
  | Qual of rqualify * ('v, 'i) exp
  | RComp of ('v, 'i) exp * comp_op * ('v, 'i) exp
  | IComp of ('v, 'i) iexp * icomp_op * ('v, 'i) iexp
  | LUn of lunop * ('v, 'i) fml
  | LBin of ('v, 'i) fml * lbinop * ('v, 'i) fml
  | QAEN of ae_quant
    * ('v, 'i) sim_binding list (** nonempty *)
    * ('v, 'i) block
  | QLO of lo_quant * (('v, 'i) binding) list * ('v, 'i) block (** nonempty *)
  | Let of (('v, 'i) binding) list * ('v, 'i) block (** nonempty *)
  | FIte of ('v, 'i) fml * ('v, 'i) fml * ('v, 'i) fml      
  | Block of ('v, 'i) block

(* simple binding *)
and ('v, 'i) binding = 'v * ('v, 'i) exp

(* simultaneous bindings to the same range *)
and ('v, 'i) sim_binding = disj * 'v list * ('v, 'i) exp (* nonempty *)

and disj = bool 

and ('v, 'i) block = ('v, 'i) fml list (** nonempty *)

and ae_quant = 
  | All 
  | Some_ 
  | No 

and lo_quant = 
  | One 
  | Lone 

and lbinop = 
  | And 
  | Or 
  | Imp 
  | Iff 
  | U
  | R                           (* release *)
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

and ('v, 'i) exp = ('v, 'i) prim_exp located

and ('v, 'i) prim_exp =
  | None_ 
  | Univ 
  | Iden 
  | Ident of 'i
  | RUn of runop * ('v, 'i) exp
      
  | RBin of ('v, 'i) exp * rbinop * ('v, 'i) exp
      
  | RIte of ('v, 'i) fml * ('v, 'i) exp * ('v, 'i) exp
  | BoxJoin of ('v, 'i) exp * ('v, 'i) exp list (** <> []  *)
  | Compr of ('v, 'i) sim_binding list * ('v, 'i) block
  | Prime of ('v, 'i) exp

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

and ('v, 'i) iexp = ('v, 'i) prim_iexp located

and ('v, 'i) prim_iexp =
  | Num of int
  | Card of ('v, 'i) exp
  | IUn of iunop * ('v, 'i) iexp
  | IBin of ('v, 'i) iexp * ibinop * ('v, 'i) iexp

and iunop =
  | Neg

and ibinop =
  | Add
  | Sub

let fbuiltin str args = FBuiltin (str, args)

let true_ = True

let false_ = False

let qual qual exp = Qual (qual, exp)

let rcomp exp1 rcomp exp2 = RComp (exp1, rcomp, exp2)

let icomp exp1 rcomp exp2 = IComp (exp1, rcomp, exp2)

let lbinary fml1 binop fml2 = LBin (fml1, binop, fml2)

let qaen quant decls block = 
  assert (decls <> [] && block <> []);
  QAEN (quant, decls, block)

let qlo quant decl block = 
  assert (block <> []);
  QLO (quant, decl, block)

let lunary lunop fml = LUn (lunop, fml)

let block block = Block block

let fite f t e = FIte (f, t, e)

let let_ decls block = Let (decls, block)

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

let release = R

let since = S

let not_ = Not

let sometime = F

let always = G

let once = O

let next = X

let historically = H

let previous = P

let num n = Num n

let none = None_

let univ = Univ

let iden = Iden

let ident x = Ident x

let runary runop exp = RUn (runop, exp)

let rbinary exp1 rbinop exp2 = RBin (exp1, rbinop, exp2)

let rite cdt then_ else_ = RIte (cdt, then_, else_)

let boxjoin caller callee = BoxJoin (caller, callee)

let compr decls block =
  assert (decls <> [] && block <> []);
  Compr (decls, block)

let prime exp = Prime exp

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

let card exp = Card exp

let iunary op exp = IUn (op, exp)

let ibinary exp op exp = IBin (exp, op, exp)

let neg = Neg

let add = Add

let sub = Sub


let fml loc data = { data; loc }

let exp loc data = { data; loc }

let iexp loc data = { data; loc }

let sat fs = Sat fs



(******************************************************************************
 *  Pretty-printing
 ****************************************************************************************)

let rec pp pp_v pp_i out (Sat fmls) =
  let open Fmtc in
  pf out "sat %a" (vbox
                   @@ list ~sep:(sp **> semi)
                   @@ hvbox2
                   @@ pp_fml pp_v pp_i) fmls

and pp_fml pp_v pp_i out fml =
  pp_prim_fml pp_v pp_i out fml.data

and pp_prim_fml pp_v pp_i out = 
  let open Fmtc in
  function
    | FBuiltin (str, args) ->
        pf out "%s%a" str (brackets @@ list @@ pp_exp pp_v pp_i) args
    | True ->
        pf out "true"
    | False ->
        pf out "false"
    | Qual (q, exp) ->
        pf out "%a %a" pp_rqualify q (pp_exp pp_v pp_i) exp
    | RComp (e1, op, e2) ->
        pf out "%a %a %a"
          (pp_exp pp_v pp_i) e1
          pp_comp_op op
          (pp_exp pp_v pp_i) e2
    | IComp (e1, op, e2) ->
        pf out "%a %a %a"
          (pp_iexp pp_v pp_i) e1
          pp_icomp_op op
          (pp_iexp pp_v pp_i) e2
    | LUn (op, fml) ->
        pf out "%a %a" pp_lunop op (pp_fml pp_v pp_i) fml
    | LBin (f1, op, f2) ->
        pf out "%a %a %a"
          (pp_fml pp_v pp_i) f1
          pp_lbinop op
          (pp_fml pp_v pp_i) f2        
    | QAEN (q, decls, blk) ->
        pf out "%a %a@ %a"
          pp_ae_quant q
          (list ~sep:(sp **> comma) @@ pp_sim_binding pp_v pp_i) decls
          (pp_block pp_v pp_i) blk        
    | QLO (q, bindings, blk) ->
        pf out "%a %a@ %a"
          pp_lo_quant q
          (list ~sep:(sp **> comma) @@ pp_binding ~sep:colon pp_v pp_i) bindings
          (pp_block pp_v pp_i) blk
    | Let (bindings, blk) ->
        pf out "let %a@ %a"
          (list ~sep:(sp **> comma) @@ pp_binding ~sep:equal pp_v pp_i) bindings
          (pp_block pp_v pp_i) blk
    | FIte (c, t, e) ->
        pf out "%a@ @[implies %a@]@ @[else %a@]"
          (pp_fml pp_v pp_i) c
          (pp_fml pp_v pp_i) t
          (pp_fml pp_v pp_i) e
    | Block fmls ->
        pp_block pp_v pp_i out fmls

and pp_block pp_v pp_i out fmls = 
  let open Fmtc in
  pf out "%a" (braces_
               @@ hvbox
               @@ list ~sep:(sp **> semi)
               @@ box2
               @@ pp_fml pp_v pp_i) fmls
  
and pp_rqualify out = 
  let open Fmtc in
  function
  | ROne -> pf out "one"
  | RLone -> pf out "lone"
  | RSome -> pf out "some"
  | RNo -> pf out "no"

and pp_comp_op out =
  let open Fmtc in
  function
    | In -> pf out "in"
    | NotIn -> pf out "not in"
    | REq -> pf out "="
    | RNEq -> pf out "!="

and pp_icomp_op out =
  let open Fmtc in
  function
    | Lt -> pf out "<"
    | IEq -> pf out "="
    | INEq -> pf out "!="
    | Lte -> pf out "<="
    | Gt -> pf out ">"
    | Gte -> pf out ">="

and pp_lunop out =
  let open Fmtc in
  function
    | Not -> pf out "not"
    | F -> pf out "sometime"
    | G -> pf out "always"
    | O -> pf out "once"
    | X -> pf out "next"
    | H -> pf out "historically"
    | P -> pf out "previous"

and pp_lbinop out =
  let open Fmtc in
  function
    | And -> pf out "and"
    | Or -> pf out "or"
    | Imp -> pf out "implies"
    | Iff -> pf out "iff"
    | U -> pf out "until"
    | R -> pf out "release"
    | S -> pf out "since"

and pp_lo_quant out =
  let open Fmtc in
  function
    | Lone -> pf out "lone"
    | One -> pf out "one"

and pp_ae_quant out =
  let open Fmtc in
  function
    | All -> pf out "all"
    | Some_ -> pf out "some"
    | No -> pf out "no"

and pp_binding ~sep pp_v pp_i out (v, e) =
  let open Fmtc in
  pf out "%a %a %a"
    pp_v v
    sep ()
    (pp_exp pp_v pp_i) e

and pp_sim_binding pp_v pp_i out (disj, vars, exp) =
  let open Fmtc in
  pf out "%a%a : %a"
    (if disj then string else nop) "disj "
    (list ~sep:(sp **> comma) pp_v) vars
    (pp_exp pp_v pp_i) exp

and pp_exp pp_v pp_i out exp =
  pp_prim_exp pp_v pp_i out exp.data

and pp_prim_exp pp_v pp_i out = 
  let open Fmtc in
  function
    | None_ ->
        pf out "none"
    | Univ ->
        pf out "univ"
    | Iden ->
        pf out "iden"
    | Ident id ->
        pf out "%a" pp_i id
    | RUn (op, e) ->
        pf out "%a%a"
          pp_runop op
          (pp_exp pp_v pp_i) e
    | RBin (e1, op, e2) ->
        pf out "%a %a@ %a"
          (pp_exp pp_v pp_i) e1
          pp_rbinop op
          (pp_exp pp_v pp_i) e2
    | RIte (c, t, e) ->
        pf out "%a@ @[implies %a@]@ @[else %a@]"
          (pp_fml pp_v pp_i) c
          (pp_exp pp_v pp_i) t
          (pp_exp pp_v pp_i) e
    | BoxJoin (exp, args) ->
        pf out "%a%a"
          (pp_exp pp_v pp_i) exp
          (brackets @@ list ~sep:(sp **> comma) @@ pp_exp pp_v pp_i) args          
    | Compr (sim_bindings, blk) ->
        pf out "%a"
          (braces_ @@
           pair ~sep:sp
             (list ~sep:(sp **> comma) @@ pp_sim_binding pp_v pp_i) 
             (pp_block pp_v pp_i))
          (sim_bindings, blk)
    | Prime e ->
        pf out "%a'" (pp_exp pp_v pp_i) e
and pp_runop out = 
  let open Fmtc in
  function
  | Transpose -> pf out "~"
  | TClos -> pf out "^"
  | RTClos -> pf out "*"

and pp_rbinop out = 
  let open Fmtc in
  function
    | Union -> pf out "+"
    | Inter -> pf out "&"
    | Over -> pf out "++"
    | LProj -> pf out "<:"
    | RProj -> pf out ":>"
    | Prod -> pf out "->"
    | Diff -> pf out "-"
    | Join -> pf out "."

and pp_iexp pp_v pp_i out iexp =  
  let open Fmtc in
  pf out "%a" (pp_prim_iexp pp_v pp_i) iexp.data

and pp_prim_iexp pp_v pp_i out = 
  let open Fmtc in
  function
    | Num n ->
        pf out "%d" n
    | Card exp ->
        pf out "#%a" (pp_exp pp_v pp_i) exp
    | IUn (op, iexp) ->
        pf out "%a%a"
          pp_iunop op
          (pp_iexp pp_v pp_i) iexp
    | IBin (e1, op, e2) -> 
        pf out "%a %a@ %a"
          (pp_iexp pp_v pp_i) e1
          pp_ibinop op
          (pp_iexp pp_v pp_i) e2

and pp_iunop out = 
  let open Fmtc in
  function
    | Neg -> pf out "-"

and pp_ibinop out = 
  let open Fmtc in
  function
    | Add -> pf out "+"
    | Sub -> pf out "-"
