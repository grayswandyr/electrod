
type 'a typed = {
  data : 'a;
  loc : Location.t;
  mutable typ : Type.t option;
}

let pp_typed pp_data out { data; typ; _ } =
  Fmtc.(pf out "%a%a" pp_data data
          (option @@ sp **< (surround (const string "«")
                               (const string "»") Type.pp)) typ)

(* ['v] is the type of variables introduced in quantifiers, ['i] is the type of
   any identifier (a variable like in the former case or a relation name) *)
type ('v, 'i) t =
  | Sat of ('v, 'i) fml list

(** Formulas and expressions *)

and ('v, 'i) fml = ('v, 'i) prim_fml typed

and ('v, 'i) prim_fml =
  | FBuiltin of string * ('v, 'i) exp list (** nonempty *)
  | True 
  | False
  | Qual of rqualify * ('v, 'i) exp
  | Comp of ('v, 'i) exp * comp_op * ('v, 'i) exp
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
  | Some 
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
  | Eq 
  | NEq 
  | Lt 
  | Lte 
  | Gt 
  | Gte 

and ('v, 'i) exp = ('v, 'i) prim_exp typed

and ('v, 'i) prim_exp =
  | RBuiltin of string * ('v, 'i) exp list (** nonempty *)
  | IBuiltin of string * ('v, 'i) exp list (** nonempty *)
  | Num of int 
  | None_ 
  | Univ 
  | Iden 
  | Int     (* the set of integers *)
  | Ident of 'i
  | RUn of runop * ('v, 'i) exp
      
  | RBin of ('v, 'i) exp * rbinop * ('v, 'i) exp
      
  | RIte of ('v, 'i) fml * ('v, 'i) exp * ('v, 'i) exp
  | BoxJoin of ('v, 'i) exp * ('v, 'i) exp list (** <> []  *)
  | Compr of ('v, 'i) sim_binding * ('v, 'i) block
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
      [@@deriving show]

let fbuiltin str args = FBuiltin (str, args)

let true_ = True

let false_ = False

let qual qual exp = Qual (qual, exp)

let comp exp1 rcomp exp2 = Comp (exp1, rcomp, exp2)

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

let some = Some

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

let rbuiltin str args = RBuiltin (str, args)

let ibuiltin str args = RBuiltin (str, args)

let num n = Num n

let none = None_

let univ = Univ

let iden = Iden

let int = Int

let ident x = Ident x

let runary runop exp = RUn (runop, exp)

let rbinary exp1 rbinop exp2 = RBin (exp1, rbinop, exp2)

let rite cdt then_ else_ = RIte (cdt, then_, else_)

let boxjoin caller callee = BoxJoin (caller, callee)

let compr (disj, vs, exp) block =
  assert (vs <> [] && block <> []);
  Compr ((disj, vs, exp), block)

let prime exp = Prime exp

let in_ = In

let not_in = NotIn

let eq = Eq

let neq = NEq

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

let fml ?(typ = None) loc data = { data; loc; typ }

let exp ?(typ = None) loc data = { data; loc; typ }

let iexp ?(typ = None) loc data = { data; loc; typ }

let sat fs = Sat fs
