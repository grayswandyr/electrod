(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2024 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Implements the type for concrete ([Raw]) and abstract ([Ast]) syntax trees, before inference of De Bruijn indices and simplification into [Elo] trees. *)

type ('v, 'i) t = private Run of (('v, 'i) block * bool option) [@@unboxed]
and ('v, 'i) fml = { prim_fml : ('v, 'i) prim_fml; fml_loc : Location.t }

and ('v, 'i) prim_fml = private
  | True
  | False
  | Qual of rqualify * ('v, 'i) exp
  | RComp of ('v, 'i) exp * comp_op * ('v, 'i) exp
  | IComp of ('v, 'i) iexp * icomp_op * ('v, 'i) iexp
  | LUn of lunop * ('v, 'i) fml
  | LBin of ('v, 'i) fml * lbinop * ('v, 'i) fml
  | Quant of quant * ('v, 'i) sim_binding list * ('v, 'i) block
  | Let of ('v, 'i) binding list * ('v, 'i) block
  | FIte of ('v, 'i) fml * ('v, 'i) fml * ('v, 'i) fml
  | Block of ('v, 'i) block

and ('v, 'i) binding = 'v * ('v, 'i) exp
and ('v, 'i) sim_binding = disj * 'v list * ('v, 'i) exp
and disj = bool
and ('v, 'i) block = ('v, 'i) fml list
and quant = private All | Some_ | No | One | Lone
and lbinop = private And | Or | Imp | Iff | U | R | S | T
and lunop = private F | G | Not | O | X | H | P
and comp_op = private In | NotIn | REq | RNEq
and icomp_op = private IEq | INEq | Lt | Lte | Gt | Gte

and ('v, 'i) exp = {
  prim_exp : ('v, 'i) prim_exp;
  exp_loc : Location.t;
  arity : int option;
}

and ('v, 'i) prim_exp = private
  | None_
  | Univ
  | Iden
  | Ident of 'i
  | RUn of runop * ('v, 'i) exp
  | RBin of ('v, 'i) exp * rbinop * ('v, 'i) exp
  | RIte of ('v, 'i) fml * ('v, 'i) exp * ('v, 'i) exp
  | BoxJoin of ('v, 'i) exp * ('v, 'i) exp list
  | Compr of ('v, 'i) sim_binding list * ('v, 'i) block
  | Prime of ('v, 'i) exp
  | Big_int of ('v, 'i) iexp

and rqualify = private ROne | RLone | RSome | RNo
and runop = private Transpose | TClos | RTClos
and rbinop = private Union | Inter | Over | LProj | RProj | Prod | Diff | Join
and ('v, 'i) iexp = { prim_iexp : ('v, 'i) prim_iexp; iexp_loc : Location.t }

and ('v, 'i) prim_iexp = private
  | Num of int
  | Card of ('v, 'i) exp
  | IUn of iunop * ('v, 'i) iexp
  | IBin of ('v, 'i) iexp * ibinop * ('v, 'i) iexp
  | Small_int of ('v, 'i) exp
  | Sum of ('v, 'i) binding list * ('v, 'i) iexp

and iunop = private Neg

and ibinop = private
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Lshift
  | Zershift
  | Sershift

class virtual ['c] map :
  object ('c)
    constraint
    'c = < visit_'i : 'd -> 'g -> 'h
         ; visit_'v : 'd -> 'i -> 'j
         ; visit_Add : 'd -> ibinop
         ; visit_All : 'd -> quant
         ; visit_And : 'd -> lbinop
         ; visit_Big_int : 'd -> ('i, 'g) iexp -> ('j, 'h) prim_exp
         ; visit_Block : 'd -> ('i, 'g) block -> ('j, 'h) prim_fml
         ; visit_BoxJoin :
             'd -> ('i, 'g) exp -> ('i, 'g) exp list -> ('j, 'h) prim_exp
         ; visit_Card : 'd -> ('i, 'g) exp -> ('j, 'h) prim_iexp
         ; visit_Compr :
             'd ->
             ('i, 'g) sim_binding list ->
             ('i, 'g) block ->
             ('j, 'h) prim_exp
         ; visit_Diff : 'd -> rbinop
         ; visit_Div : 'd -> ibinop
         ; visit_F : 'd -> lunop
         ; visit_FIte :
             'd ->
             ('i, 'g) fml ->
             ('i, 'g) fml ->
             ('i, 'g) fml ->
             ('j, 'h) prim_fml
         ; visit_False : 'd -> ('j, 'h) prim_fml
         ; visit_G : 'd -> lunop
         ; visit_Gt : 'd -> icomp_op
         ; visit_Gte : 'd -> icomp_op
         ; visit_H : 'd -> lunop
         ; visit_IBin :
             'd ->
             ('i, 'g) iexp ->
             ibinop ->
             ('i, 'g) iexp ->
             ('j, 'h) prim_iexp
         ; visit_IComp :
             'd ->
             ('i, 'g) iexp ->
             icomp_op ->
             ('i, 'g) iexp ->
             ('j, 'h) prim_fml
         ; visit_IEq : 'd -> icomp_op
         ; visit_INEq : 'd -> icomp_op
         ; visit_IUn : 'd -> iunop -> ('i, 'g) iexp -> ('j, 'h) prim_iexp
         ; visit_Iden : 'd -> ('j, 'h) prim_exp
         ; visit_Ident : 'd -> 'g -> ('j, 'h) prim_exp
         ; visit_Iff : 'd -> lbinop
         ; visit_Imp : 'd -> lbinop
         ; visit_In : 'd -> comp_op
         ; visit_Inter : 'd -> rbinop
         ; visit_Join : 'd -> rbinop
         ; visit_LBin :
             'd -> ('i, 'g) fml -> lbinop -> ('i, 'g) fml -> ('j, 'h) prim_fml
         ; visit_LProj : 'd -> rbinop
         ; visit_LUn : 'd -> lunop -> ('i, 'g) fml -> ('j, 'h) prim_fml
         ; visit_Let :
             'd -> ('i, 'g) binding list -> ('i, 'g) block -> ('j, 'h) prim_fml
         ; visit_Lone : 'd -> quant
         ; visit_Lshift : 'd -> ibinop
         ; visit_Lt : 'd -> icomp_op
         ; visit_Lte : 'd -> icomp_op
         ; visit_Mul : 'd -> ibinop
         ; visit_Neg : 'd -> iunop
         ; visit_No : 'd -> quant
         ; visit_None_ : 'd -> ('j, 'h) prim_exp
         ; visit_Not : 'd -> lunop
         ; visit_NotIn : 'd -> comp_op
         ; visit_Num : 'd -> int -> ('j, 'h) prim_iexp
         ; visit_O : 'd -> lunop
         ; visit_One : 'd -> quant
         ; visit_Or : 'd -> lbinop
         ; visit_Over : 'd -> rbinop
         ; visit_P : 'd -> lunop
         ; visit_Prime : 'd -> ('i, 'g) exp -> ('j, 'h) prim_exp
         ; visit_Prod : 'd -> rbinop
         ; visit_Qual : 'd -> rqualify -> ('i, 'g) exp -> ('j, 'h) prim_fml
         ; visit_Quant :
             'd ->
             quant ->
             ('i, 'g) sim_binding list ->
             ('i, 'g) block ->
             ('j, 'h) prim_fml
         ; visit_R : 'd -> lbinop
         ; visit_RBin :
             'd -> ('i, 'g) exp -> rbinop -> ('i, 'g) exp -> ('j, 'h) prim_exp
         ; visit_RComp :
             'd -> ('i, 'g) exp -> comp_op -> ('i, 'g) exp -> ('j, 'h) prim_fml
         ; visit_REq : 'd -> comp_op
         ; visit_RIte :
             'd ->
             ('i, 'g) fml ->
             ('i, 'g) exp ->
             ('i, 'g) exp ->
             ('j, 'h) prim_exp
         ; visit_RLone : 'd -> rqualify
         ; visit_RNEq : 'd -> comp_op
         ; visit_RNo : 'd -> rqualify
         ; visit_ROne : 'd -> rqualify
         ; visit_RProj : 'd -> rbinop
         ; visit_RSome : 'd -> rqualify
         ; visit_RTClos : 'd -> runop
         ; visit_RUn : 'd -> runop -> ('i, 'g) exp -> ('j, 'h) prim_exp
         ; visit_Rem : 'd -> ibinop
         ; visit_Run : 'd -> ('i, 'g) block * disj option -> ('j, 'h) t
         ; visit_S : 'd -> lbinop
         ; visit_Sershift : 'd -> ibinop
         ; visit_Small_int : 'd -> ('i, 'g) exp -> ('j, 'h) prim_iexp
         ; visit_Some_ : 'd -> quant
         ; visit_Sub : 'd -> ibinop
         ; visit_Sum :
             'd -> ('i, 'g) binding list -> ('i, 'g) iexp -> ('j, 'h) prim_iexp
         ; visit_T : 'd -> lbinop
         ; visit_TClos : 'd -> runop
         ; visit_Transpose : 'd -> runop
         ; visit_True : 'd -> ('j, 'h) prim_fml
         ; visit_U : 'd -> lbinop
         ; visit_Union : 'd -> rbinop
         ; visit_Univ : 'd -> ('j, 'h) prim_exp
         ; visit_X : 'd -> lunop
         ; visit_Zershift : 'd -> ibinop
         ; visit_binding : 'd -> ('i, 'g) binding -> 'j * ('j, 'h) exp
         ; visit_block : 'd -> ('i, 'g) block -> ('j, 'h) block
         ; visit_comp_op : 'd -> comp_op -> comp_op
         ; visit_disj : 'd -> disj -> disj
         ; visit_exp : 'd -> ('i, 'g) exp -> ('j, 'h) exp
         ; visit_fml : 'd -> ('i, 'g) fml -> ('j, 'h) fml
         ; visit_ibinop : 'd -> ibinop -> ibinop
         ; visit_icomp_op : 'd -> icomp_op -> icomp_op
         ; visit_iexp : 'd -> ('i, 'g) iexp -> ('j, 'h) iexp
         ; visit_iunop : 'd -> iunop -> iunop
         ; visit_lbinop : 'd -> lbinop -> lbinop
         ; visit_lunop : 'd -> lunop -> lunop
         ; visit_prim_exp : 'd -> ('i, 'g) prim_exp -> ('j, 'h) prim_exp
         ; visit_prim_fml : 'd -> ('i, 'g) prim_fml -> ('j, 'h) prim_fml
         ; visit_prim_iexp : 'd -> ('i, 'g) prim_iexp -> ('j, 'h) prim_iexp
         ; visit_quant : 'd -> quant -> quant
         ; visit_rbinop : 'd -> rbinop -> rbinop
         ; visit_rqualify : 'd -> rqualify -> rqualify
         ; visit_runop : 'd -> runop -> runop
         ; visit_sim_binding :
             'd -> ('i, 'g) sim_binding -> disj * 'j list * ('j, 'h) exp
         ; visit_t : 'd -> ('i, 'g) t -> ('j, 'h) t
         ; .. >

    method virtual visit_'i : 'd -> 'g -> 'h
    method virtual visit_'v : 'd -> 'i -> 'j
    method visit_Add : 'd -> ibinop
    method visit_All : 'd -> quant
    method visit_And : 'd -> lbinop
    method visit_Big_int : 'd -> ('i, 'g) iexp -> ('j, 'h) prim_exp
    method visit_Block : 'd -> ('i, 'g) block -> ('j, 'h) prim_fml

    method visit_BoxJoin :
      'd -> ('i, 'g) exp -> ('i, 'g) exp list -> ('j, 'h) prim_exp

    method visit_Card : 'd -> ('i, 'g) exp -> ('j, 'h) prim_iexp

    method visit_Compr :
      'd -> ('i, 'g) sim_binding list -> ('i, 'g) block -> ('j, 'h) prim_exp

    method visit_Diff : 'd -> rbinop
    method visit_Div : 'd -> ibinop
    method visit_F : 'd -> lunop

    method visit_FIte :
      'd -> ('i, 'g) fml -> ('i, 'g) fml -> ('i, 'g) fml -> ('j, 'h) prim_fml

    method visit_False : 'd -> ('j, 'h) prim_fml
    method visit_G : 'd -> lunop
    method visit_Gt : 'd -> icomp_op
    method visit_Gte : 'd -> icomp_op
    method visit_H : 'd -> lunop

    method visit_IBin :
      'd -> ('i, 'g) iexp -> ibinop -> ('i, 'g) iexp -> ('j, 'h) prim_iexp

    method visit_IComp :
      'd -> ('i, 'g) iexp -> icomp_op -> ('i, 'g) iexp -> ('j, 'h) prim_fml

    method visit_IEq : 'd -> icomp_op
    method visit_INEq : 'd -> icomp_op
    method visit_IUn : 'd -> iunop -> ('i, 'g) iexp -> ('j, 'h) prim_iexp
    method visit_Iden : 'd -> ('j, 'h) prim_exp
    method visit_Ident : 'd -> 'g -> ('j, 'h) prim_exp
    method visit_Iff : 'd -> lbinop
    method visit_Imp : 'd -> lbinop
    method visit_In : 'd -> comp_op
    method visit_Inter : 'd -> rbinop
    method visit_Join : 'd -> rbinop

    method visit_LBin :
      'd -> ('i, 'g) fml -> lbinop -> ('i, 'g) fml -> ('j, 'h) prim_fml

    method visit_LProj : 'd -> rbinop
    method visit_LUn : 'd -> lunop -> ('i, 'g) fml -> ('j, 'h) prim_fml

    method visit_Let :
      'd -> ('i, 'g) binding list -> ('i, 'g) block -> ('j, 'h) prim_fml

    method visit_Lone : 'd -> quant
    method visit_Lshift : 'd -> ibinop
    method visit_Lt : 'd -> icomp_op
    method visit_Lte : 'd -> icomp_op
    method visit_Mul : 'd -> ibinop
    method visit_Neg : 'd -> iunop
    method visit_No : 'd -> quant
    method visit_None_ : 'd -> ('j, 'h) prim_exp
    method visit_Not : 'd -> lunop
    method visit_NotIn : 'd -> comp_op
    method visit_Num : 'd -> int -> ('j, 'h) prim_iexp
    method visit_O : 'd -> lunop
    method visit_One : 'd -> quant
    method visit_Or : 'd -> lbinop
    method visit_Over : 'd -> rbinop
    method visit_P : 'd -> lunop
    method visit_Prime : 'd -> ('i, 'g) exp -> ('j, 'h) prim_exp
    method visit_Prod : 'd -> rbinop
    method visit_Qual : 'd -> rqualify -> ('i, 'g) exp -> ('j, 'h) prim_fml

    method visit_Quant :
      'd ->
      quant ->
      ('i, 'g) sim_binding list ->
      ('i, 'g) block ->
      ('j, 'h) prim_fml

    method visit_R : 'd -> lbinop

    method visit_RBin :
      'd -> ('i, 'g) exp -> rbinop -> ('i, 'g) exp -> ('j, 'h) prim_exp

    method visit_RComp :
      'd -> ('i, 'g) exp -> comp_op -> ('i, 'g) exp -> ('j, 'h) prim_fml

    method visit_REq : 'd -> comp_op

    method visit_RIte :
      'd -> ('i, 'g) fml -> ('i, 'g) exp -> ('i, 'g) exp -> ('j, 'h) prim_exp

    method visit_RLone : 'd -> rqualify
    method visit_RNEq : 'd -> comp_op
    method visit_RNo : 'd -> rqualify
    method visit_ROne : 'd -> rqualify
    method visit_RProj : 'd -> rbinop
    method visit_RSome : 'd -> rqualify
    method visit_RTClos : 'd -> runop
    method visit_RUn : 'd -> runop -> ('i, 'g) exp -> ('j, 'h) prim_exp
    method visit_Rem : 'd -> ibinop
    method visit_Run : 'd -> ('i, 'g) block * disj option -> ('j, 'h) t
    method visit_S : 'd -> lbinop
    method visit_Sershift : 'd -> ibinop
    method visit_Small_int : 'd -> ('i, 'g) exp -> ('j, 'h) prim_iexp
    method visit_Some_ : 'd -> quant
    method visit_Sub : 'd -> ibinop

    method visit_Sum :
      'd -> ('i, 'g) binding list -> ('i, 'g) iexp -> ('j, 'h) prim_iexp

    method visit_T : 'd -> lbinop
    method visit_TClos : 'd -> runop
    method visit_Transpose : 'd -> runop
    method visit_True : 'd -> ('j, 'h) prim_fml
    method visit_U : 'd -> lbinop
    method visit_Union : 'd -> rbinop
    method visit_Univ : 'd -> ('j, 'h) prim_exp
    method visit_X : 'd -> lunop
    method visit_Zershift : 'd -> ibinop

    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

    method visit_binding : 'd -> ('i, 'g) binding -> 'j * ('j, 'h) exp
    method visit_block : 'd -> ('i, 'g) block -> ('j, 'h) block
    method private visit_bool : 'env. 'env -> disj -> disj
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_comp_op : 'd -> comp_op -> comp_op
    method visit_disj : 'd -> disj -> disj
    method visit_exp : 'd -> ('i, 'g) exp -> ('j, 'h) exp
    method private visit_float : 'env. 'env -> float -> float
    method visit_fml : 'd -> ('i, 'g) fml -> ('j, 'h) fml
    method visit_ibinop : 'd -> ibinop -> ibinop
    method visit_icomp_op : 'd -> icomp_op -> icomp_op
    method visit_iexp : 'd -> ('i, 'g) iexp -> ('j, 'h) iexp
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_iunop : 'd -> iunop -> iunop

    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t

    method visit_lbinop : 'd -> lbinop -> lbinop

    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method visit_lunop : 'd -> lunop -> lunop
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    method visit_prim_exp : 'd -> ('i, 'g) prim_exp -> ('j, 'h) prim_exp
    method visit_prim_fml : 'd -> ('i, 'g) prim_fml -> ('j, 'h) prim_fml
    method visit_prim_iexp : 'd -> ('i, 'g) prim_iexp -> ('j, 'h) prim_iexp
    method visit_quant : 'd -> quant -> quant
    method visit_rbinop : 'd -> rbinop -> rbinop

    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

    method private visit_result :
      'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b) ->
      ('env -> 'e -> 'f) ->
      'env ->
      ('a, 'e) result ->
      ('b, 'f) result

    method visit_rqualify : 'd -> rqualify -> rqualify
    method visit_runop : 'd -> runop -> runop

    method visit_sim_binding :
      'd -> ('i, 'g) sim_binding -> disj * 'j list * ('j, 'h) exp

    method private visit_string : 'env. 'env -> string -> string
    method visit_t : 'd -> ('i, 'g) t -> ('j, 'h) t
    method private visit_unit : 'env. 'env -> unit -> unit
  end

class virtual ['c] fold :
  object ('c)
    constraint
    'c = < build_Add : 'd -> 'g
         ; build_All : 'd -> 'h
         ; build_And : 'd -> 'i
         ; build_Big_int : 'd -> 'j -> 'k
         ; build_Block : 'd -> 'l list -> 'm
         ; build_BoxJoin : 'd -> 'n -> 'n list -> 'k
         ; build_Card : 'd -> 'n -> 'o
         ; build_Compr : 'd -> (disj * 'p list * 'n) list -> 'l list -> 'k
         ; build_Diff : 'd -> 'q
         ; build_Div : 'd -> 'g
         ; build_F : 'd -> 'r
         ; build_FIte : 'd -> 'l -> 'l -> 'l -> 'm
         ; build_False : 'd -> 'm
         ; build_G : 'd -> 'r
         ; build_Gt : 'd -> 's
         ; build_Gte : 'd -> 's
         ; build_H : 'd -> 'r
         ; build_IBin : 'd -> 'j -> 'g -> 'j -> 'o
         ; build_IComp : 'd -> 'j -> 's -> 'j -> 'm
         ; build_IEq : 'd -> 's
         ; build_INEq : 'd -> 's
         ; build_IUn : 'd -> 't -> 'j -> 'o
         ; build_Iden : 'd -> 'k
         ; build_Ident : 'd -> 'u -> 'k
         ; build_Iff : 'd -> 'i
         ; build_Imp : 'd -> 'i
         ; build_In : 'd -> 'v
         ; build_Inter : 'd -> 'q
         ; build_Join : 'd -> 'q
         ; build_LBin : 'd -> 'l -> 'i -> 'l -> 'm
         ; build_LProj : 'd -> 'q
         ; build_LUn : 'd -> 'r -> 'l -> 'm
         ; build_Let : 'd -> ('p * 'n) list -> 'l list -> 'm
         ; build_Lone : 'd -> 'h
         ; build_Lshift : 'd -> 'g
         ; build_Lt : 'd -> 's
         ; build_Lte : 'd -> 's
         ; build_Mul : 'd -> 'g
         ; build_Neg : 'd -> 't
         ; build_No : 'd -> 'h
         ; build_None_ : 'd -> 'k
         ; build_Not : 'd -> 'r
         ; build_NotIn : 'd -> 'v
         ; build_Num : 'd -> int -> 'o
         ; build_O : 'd -> 'r
         ; build_One : 'd -> 'h
         ; build_Or : 'd -> 'i
         ; build_Over : 'd -> 'q
         ; build_P : 'd -> 'r
         ; build_Prime : 'd -> 'n -> 'k
         ; build_Prod : 'd -> 'q
         ; build_Qual : 'd -> 'w -> 'n -> 'm
         ; build_Quant : 'd -> 'h -> (disj * 'p list * 'n) list -> 'l list -> 'm
         ; build_R : 'd -> 'i
         ; build_RBin : 'd -> 'n -> 'q -> 'n -> 'k
         ; build_RComp : 'd -> 'n -> 'v -> 'n -> 'm
         ; build_REq : 'd -> 'v
         ; build_RIte : 'd -> 'l -> 'n -> 'n -> 'k
         ; build_RLone : 'd -> 'w
         ; build_RNEq : 'd -> 'v
         ; build_RNo : 'd -> 'w
         ; build_ROne : 'd -> 'w
         ; build_RProj : 'd -> 'q
         ; build_RSome : 'd -> 'w
         ; build_RTClos : 'd -> 'x
         ; build_RUn : 'd -> 'x -> 'n -> 'k
         ; build_Rem : 'd -> 'g
         ; build_Run : 'd -> 'l list * disj option -> 'y
         ; build_S : 'd -> 'i
         ; build_Sershift : 'd -> 'g
         ; build_Small_int : 'd -> 'n -> 'o
         ; build_Some_ : 'd -> 'h
         ; build_Sub : 'd -> 'g
         ; build_Sum : 'd -> ('p * 'n) list -> 'j -> 'o
         ; build_T : 'd -> 'i
         ; build_TClos : 'd -> 'x
         ; build_Transpose : 'd -> 'x
         ; build_True : 'd -> 'm
         ; build_U : 'd -> 'i
         ; build_Union : 'd -> 'q
         ; build_Univ : 'd -> 'k
         ; build_X : 'd -> 'r
         ; build_Zershift : 'd -> 'g
         ; build_exp : 'd -> 'k -> Location.t -> int option -> 'n
         ; build_fml : 'd -> 'm -> Location.t -> 'l
         ; build_iexp : 'd -> 'o -> Location.t -> 'j
         ; visit_'i : 'd -> 'z -> 'u
         ; visit_'v : 'd -> 'a1 -> 'p
         ; visit_Add : 'd -> 'g
         ; visit_All : 'd -> 'h
         ; visit_And : 'd -> 'i
         ; visit_Big_int : 'd -> ('a1, 'z) iexp -> 'k
         ; visit_Block : 'd -> ('a1, 'z) block -> 'm
         ; visit_BoxJoin : 'd -> ('a1, 'z) exp -> ('a1, 'z) exp list -> 'k
         ; visit_Card : 'd -> ('a1, 'z) exp -> 'o
         ; visit_Compr :
             'd -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'k
         ; visit_Diff : 'd -> 'q
         ; visit_Div : 'd -> 'g
         ; visit_F : 'd -> 'r
         ; visit_FIte :
             'd -> ('a1, 'z) fml -> ('a1, 'z) fml -> ('a1, 'z) fml -> 'm
         ; visit_False : 'd -> 'm
         ; visit_G : 'd -> 'r
         ; visit_Gt : 'd -> 's
         ; visit_Gte : 'd -> 's
         ; visit_H : 'd -> 'r
         ; visit_IBin : 'd -> ('a1, 'z) iexp -> ibinop -> ('a1, 'z) iexp -> 'o
         ; visit_IComp :
             'd -> ('a1, 'z) iexp -> icomp_op -> ('a1, 'z) iexp -> 'm
         ; visit_IEq : 'd -> 's
         ; visit_INEq : 'd -> 's
         ; visit_IUn : 'd -> iunop -> ('a1, 'z) iexp -> 'o
         ; visit_Iden : 'd -> 'k
         ; visit_Ident : 'd -> 'z -> 'k
         ; visit_Iff : 'd -> 'i
         ; visit_Imp : 'd -> 'i
         ; visit_In : 'd -> 'v
         ; visit_Inter : 'd -> 'q
         ; visit_Join : 'd -> 'q
         ; visit_LBin : 'd -> ('a1, 'z) fml -> lbinop -> ('a1, 'z) fml -> 'm
         ; visit_LProj : 'd -> 'q
         ; visit_LUn : 'd -> lunop -> ('a1, 'z) fml -> 'm
         ; visit_Let : 'd -> ('a1, 'z) binding list -> ('a1, 'z) block -> 'm
         ; visit_Lone : 'd -> 'h
         ; visit_Lshift : 'd -> 'g
         ; visit_Lt : 'd -> 's
         ; visit_Lte : 'd -> 's
         ; visit_Mul : 'd -> 'g
         ; visit_Neg : 'd -> 't
         ; visit_No : 'd -> 'h
         ; visit_None_ : 'd -> 'k
         ; visit_Not : 'd -> 'r
         ; visit_NotIn : 'd -> 'v
         ; visit_Num : 'd -> int -> 'o
         ; visit_O : 'd -> 'r
         ; visit_One : 'd -> 'h
         ; visit_Or : 'd -> 'i
         ; visit_Over : 'd -> 'q
         ; visit_P : 'd -> 'r
         ; visit_Prime : 'd -> ('a1, 'z) exp -> 'k
         ; visit_Prod : 'd -> 'q
         ; visit_Qual : 'd -> rqualify -> ('a1, 'z) exp -> 'm
         ; visit_Quant :
             'd -> quant -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'm
         ; visit_R : 'd -> 'i
         ; visit_RBin : 'd -> ('a1, 'z) exp -> rbinop -> ('a1, 'z) exp -> 'k
         ; visit_RComp : 'd -> ('a1, 'z) exp -> comp_op -> ('a1, 'z) exp -> 'm
         ; visit_REq : 'd -> 'v
         ; visit_RIte :
             'd -> ('a1, 'z) fml -> ('a1, 'z) exp -> ('a1, 'z) exp -> 'k
         ; visit_RLone : 'd -> 'w
         ; visit_RNEq : 'd -> 'v
         ; visit_RNo : 'd -> 'w
         ; visit_ROne : 'd -> 'w
         ; visit_RProj : 'd -> 'q
         ; visit_RSome : 'd -> 'w
         ; visit_RTClos : 'd -> 'x
         ; visit_RUn : 'd -> runop -> ('a1, 'z) exp -> 'k
         ; visit_Rem : 'd -> 'g
         ; visit_Run : 'd -> ('a1, 'z) block * disj option -> 'y
         ; visit_S : 'd -> 'i
         ; visit_Sershift : 'd -> 'g
         ; visit_Small_int : 'd -> ('a1, 'z) exp -> 'o
         ; visit_Some_ : 'd -> 'h
         ; visit_Sub : 'd -> 'g
         ; visit_Sum : 'd -> ('a1, 'z) binding list -> ('a1, 'z) iexp -> 'o
         ; visit_T : 'd -> 'i
         ; visit_TClos : 'd -> 'x
         ; visit_Transpose : 'd -> 'x
         ; visit_True : 'd -> 'm
         ; visit_U : 'd -> 'i
         ; visit_Union : 'd -> 'q
         ; visit_Univ : 'd -> 'k
         ; visit_X : 'd -> 'r
         ; visit_Zershift : 'd -> 'g
         ; visit_binding : 'd -> ('a1, 'z) binding -> 'p * 'n
         ; visit_block : 'd -> ('a1, 'z) block -> 'l list
         ; visit_comp_op : 'd -> comp_op -> 'v
         ; visit_disj : 'd -> disj -> disj
         ; visit_exp : 'd -> ('a1, 'z) exp -> 'n
         ; visit_fml : 'd -> ('a1, 'z) fml -> 'l
         ; visit_ibinop : 'd -> ibinop -> 'g
         ; visit_icomp_op : 'd -> icomp_op -> 's
         ; visit_iexp : 'd -> ('a1, 'z) iexp -> 'j
         ; visit_iunop : 'd -> iunop -> 't
         ; visit_lbinop : 'd -> lbinop -> 'i
         ; visit_lunop : 'd -> lunop -> 'r
         ; visit_prim_exp : 'd -> ('a1, 'z) prim_exp -> 'k
         ; visit_prim_fml : 'd -> ('a1, 'z) prim_fml -> 'm
         ; visit_prim_iexp : 'd -> ('a1, 'z) prim_iexp -> 'o
         ; visit_quant : 'd -> quant -> 'h
         ; visit_rbinop : 'd -> rbinop -> 'q
         ; visit_rqualify : 'd -> rqualify -> 'w
         ; visit_runop : 'd -> runop -> 'x
         ; visit_sim_binding :
             'd -> ('a1, 'z) sim_binding -> disj * 'p list * 'n
         ; visit_t : 'd -> ('a1, 'z) t -> 'y
         ; .. >

    method virtual build_Add : 'd -> 'g
    method virtual build_All : 'd -> 'h
    method virtual build_And : 'd -> 'i
    method virtual build_Big_int : 'd -> 'j -> 'k
    method virtual build_Block : 'd -> 'l list -> 'm
    method virtual build_BoxJoin : 'd -> 'n -> 'n list -> 'k
    method virtual build_Card : 'd -> 'n -> 'o

    method virtual build_Compr :
      'd -> (disj * 'p list * 'n) list -> 'l list -> 'k

    method virtual build_Diff : 'd -> 'q
    method virtual build_Div : 'd -> 'g
    method virtual build_F : 'd -> 'r
    method virtual build_FIte : 'd -> 'l -> 'l -> 'l -> 'm
    method virtual build_False : 'd -> 'm
    method virtual build_G : 'd -> 'r
    method virtual build_Gt : 'd -> 's
    method virtual build_Gte : 'd -> 's
    method virtual build_H : 'd -> 'r
    method virtual build_IBin : 'd -> 'j -> 'g -> 'j -> 'o
    method virtual build_IComp : 'd -> 'j -> 's -> 'j -> 'm
    method virtual build_IEq : 'd -> 's
    method virtual build_INEq : 'd -> 's
    method virtual build_IUn : 'd -> 't -> 'j -> 'o
    method virtual build_Iden : 'd -> 'k
    method virtual build_Ident : 'd -> 'u -> 'k
    method virtual build_Iff : 'd -> 'i
    method virtual build_Imp : 'd -> 'i
    method virtual build_In : 'd -> 'v
    method virtual build_Inter : 'd -> 'q
    method virtual build_Join : 'd -> 'q
    method virtual build_LBin : 'd -> 'l -> 'i -> 'l -> 'm
    method virtual build_LProj : 'd -> 'q
    method virtual build_LUn : 'd -> 'r -> 'l -> 'm
    method virtual build_Let : 'd -> ('p * 'n) list -> 'l list -> 'm
    method virtual build_Lone : 'd -> 'h
    method virtual build_Lshift : 'd -> 'g
    method virtual build_Lt : 'd -> 's
    method virtual build_Lte : 'd -> 's
    method virtual build_Mul : 'd -> 'g
    method virtual build_Neg : 'd -> 't
    method virtual build_No : 'd -> 'h
    method virtual build_None_ : 'd -> 'k
    method virtual build_Not : 'd -> 'r
    method virtual build_NotIn : 'd -> 'v
    method virtual build_Num : 'd -> int -> 'o
    method virtual build_O : 'd -> 'r
    method virtual build_One : 'd -> 'h
    method virtual build_Or : 'd -> 'i
    method virtual build_Over : 'd -> 'q
    method virtual build_P : 'd -> 'r
    method virtual build_Prime : 'd -> 'n -> 'k
    method virtual build_Prod : 'd -> 'q
    method virtual build_Qual : 'd -> 'w -> 'n -> 'm

    method virtual build_Quant :
      'd -> 'h -> (disj * 'p list * 'n) list -> 'l list -> 'm

    method virtual build_R : 'd -> 'i
    method virtual build_RBin : 'd -> 'n -> 'q -> 'n -> 'k
    method virtual build_RComp : 'd -> 'n -> 'v -> 'n -> 'm
    method virtual build_REq : 'd -> 'v
    method virtual build_RIte : 'd -> 'l -> 'n -> 'n -> 'k
    method virtual build_RLone : 'd -> 'w
    method virtual build_RNEq : 'd -> 'v
    method virtual build_RNo : 'd -> 'w
    method virtual build_ROne : 'd -> 'w
    method virtual build_RProj : 'd -> 'q
    method virtual build_RSome : 'd -> 'w
    method virtual build_RTClos : 'd -> 'x
    method virtual build_RUn : 'd -> 'x -> 'n -> 'k
    method virtual build_Rem : 'd -> 'g
    method virtual build_Run : 'd -> 'l list * disj option -> 'y
    method virtual build_S : 'd -> 'i
    method virtual build_Sershift : 'd -> 'g
    method virtual build_Small_int : 'd -> 'n -> 'o
    method virtual build_Some_ : 'd -> 'h
    method virtual build_Sub : 'd -> 'g
    method virtual build_Sum : 'd -> ('p * 'n) list -> 'j -> 'o
    method virtual build_T : 'd -> 'i
    method virtual build_TClos : 'd -> 'x
    method virtual build_Transpose : 'd -> 'x
    method virtual build_True : 'd -> 'm
    method virtual build_U : 'd -> 'i
    method virtual build_Union : 'd -> 'q
    method virtual build_Univ : 'd -> 'k
    method virtual build_X : 'd -> 'r
    method virtual build_Zershift : 'd -> 'g
    method virtual build_exp : 'd -> 'k -> Location.t -> int option -> 'n
    method virtual build_fml : 'd -> 'm -> Location.t -> 'l
    method virtual build_iexp : 'd -> 'o -> Location.t -> 'j
    method virtual visit_'i : 'd -> 'z -> 'u
    method virtual visit_'v : 'd -> 'a1 -> 'p
    method visit_Add : 'd -> 'g
    method visit_All : 'd -> 'h
    method visit_And : 'd -> 'i
    method visit_Big_int : 'd -> ('a1, 'z) iexp -> 'k
    method visit_Block : 'd -> ('a1, 'z) block -> 'm
    method visit_BoxJoin : 'd -> ('a1, 'z) exp -> ('a1, 'z) exp list -> 'k
    method visit_Card : 'd -> ('a1, 'z) exp -> 'o

    method visit_Compr :
      'd -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'k

    method visit_Diff : 'd -> 'q
    method visit_Div : 'd -> 'g
    method visit_F : 'd -> 'r

    method visit_FIte :
      'd -> ('a1, 'z) fml -> ('a1, 'z) fml -> ('a1, 'z) fml -> 'm

    method visit_False : 'd -> 'm
    method visit_G : 'd -> 'r
    method visit_Gt : 'd -> 's
    method visit_Gte : 'd -> 's
    method visit_H : 'd -> 'r
    method visit_IBin : 'd -> ('a1, 'z) iexp -> ibinop -> ('a1, 'z) iexp -> 'o

    method visit_IComp :
      'd -> ('a1, 'z) iexp -> icomp_op -> ('a1, 'z) iexp -> 'm

    method visit_IEq : 'd -> 's
    method visit_INEq : 'd -> 's
    method visit_IUn : 'd -> iunop -> ('a1, 'z) iexp -> 'o
    method visit_Iden : 'd -> 'k
    method visit_Ident : 'd -> 'z -> 'k
    method visit_Iff : 'd -> 'i
    method visit_Imp : 'd -> 'i
    method visit_In : 'd -> 'v
    method visit_Inter : 'd -> 'q
    method visit_Join : 'd -> 'q
    method visit_LBin : 'd -> ('a1, 'z) fml -> lbinop -> ('a1, 'z) fml -> 'm
    method visit_LProj : 'd -> 'q
    method visit_LUn : 'd -> lunop -> ('a1, 'z) fml -> 'm
    method visit_Let : 'd -> ('a1, 'z) binding list -> ('a1, 'z) block -> 'm
    method visit_Lone : 'd -> 'h
    method visit_Lshift : 'd -> 'g
    method visit_Lt : 'd -> 's
    method visit_Lte : 'd -> 's
    method visit_Mul : 'd -> 'g
    method visit_Neg : 'd -> 't
    method visit_No : 'd -> 'h
    method visit_None_ : 'd -> 'k
    method visit_Not : 'd -> 'r
    method visit_NotIn : 'd -> 'v
    method visit_Num : 'd -> int -> 'o
    method visit_O : 'd -> 'r
    method visit_One : 'd -> 'h
    method visit_Or : 'd -> 'i
    method visit_Over : 'd -> 'q
    method visit_P : 'd -> 'r
    method visit_Prime : 'd -> ('a1, 'z) exp -> 'k
    method visit_Prod : 'd -> 'q
    method visit_Qual : 'd -> rqualify -> ('a1, 'z) exp -> 'm

    method visit_Quant :
      'd -> quant -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'm

    method visit_R : 'd -> 'i
    method visit_RBin : 'd -> ('a1, 'z) exp -> rbinop -> ('a1, 'z) exp -> 'k
    method visit_RComp : 'd -> ('a1, 'z) exp -> comp_op -> ('a1, 'z) exp -> 'm
    method visit_REq : 'd -> 'v

    method visit_RIte :
      'd -> ('a1, 'z) fml -> ('a1, 'z) exp -> ('a1, 'z) exp -> 'k

    method visit_RLone : 'd -> 'w
    method visit_RNEq : 'd -> 'v
    method visit_RNo : 'd -> 'w
    method visit_ROne : 'd -> 'w
    method visit_RProj : 'd -> 'q
    method visit_RSome : 'd -> 'w
    method visit_RTClos : 'd -> 'x
    method visit_RUn : 'd -> runop -> ('a1, 'z) exp -> 'k
    method visit_Rem : 'd -> 'g
    method visit_Run : 'd -> ('a1, 'z) block * disj option -> 'y
    method visit_S : 'd -> 'i
    method visit_Sershift : 'd -> 'g
    method visit_Small_int : 'd -> ('a1, 'z) exp -> 'o
    method visit_Some_ : 'd -> 'h
    method visit_Sub : 'd -> 'g
    method visit_Sum : 'd -> ('a1, 'z) binding list -> ('a1, 'z) iexp -> 'o
    method visit_T : 'd -> 'i
    method visit_TClos : 'd -> 'x
    method visit_Transpose : 'd -> 'x
    method visit_True : 'd -> 'm
    method visit_U : 'd -> 'i
    method visit_Union : 'd -> 'q
    method visit_Univ : 'd -> 'k
    method visit_X : 'd -> 'r
    method visit_Zershift : 'd -> 'g

    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

    method visit_binding : 'd -> ('a1, 'z) binding -> 'p * 'n
    method visit_block : 'd -> ('a1, 'z) block -> 'l list
    method private visit_bool : 'env. 'env -> disj -> disj
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_comp_op : 'd -> comp_op -> 'v
    method visit_disj : 'd -> disj -> disj
    method visit_exp : 'd -> ('a1, 'z) exp -> 'n
    method private visit_float : 'env. 'env -> float -> float
    method visit_fml : 'd -> ('a1, 'z) fml -> 'l
    method visit_ibinop : 'd -> ibinop -> 'g
    method visit_icomp_op : 'd -> icomp_op -> 's
    method visit_iexp : 'd -> ('a1, 'z) iexp -> 'j
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_iunop : 'd -> iunop -> 't

    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t

    method visit_lbinop : 'd -> lbinop -> 'i

    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method visit_lunop : 'd -> lunop -> 'r
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    method visit_prim_exp : 'd -> ('a1, 'z) prim_exp -> 'k
    method visit_prim_fml : 'd -> ('a1, 'z) prim_fml -> 'm
    method visit_prim_iexp : 'd -> ('a1, 'z) prim_iexp -> 'o
    method visit_quant : 'd -> quant -> 'h
    method visit_rbinop : 'd -> rbinop -> 'q

    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

    method private visit_result :
      'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b) ->
      ('env -> 'e -> 'f) ->
      'env ->
      ('a, 'e) result ->
      ('b, 'f) result

    method visit_rqualify : 'd -> rqualify -> 'w
    method visit_runop : 'd -> runop -> 'x

    method visit_sim_binding :
      'd -> ('a1, 'z) sim_binding -> disj * 'p list * 'n

    method private visit_string : 'env. 'env -> string -> string
    method visit_t : 'd -> ('a1, 'z) t -> 'y
    method private visit_unit : 'env. 'env -> unit -> unit
  end

val true_ : ('a, 'b) prim_fml
val false_ : ('a, 'b) prim_fml
val qual : rqualify -> ('a, 'b) exp -> ('a, 'b) prim_fml
val rcomp : ('a, 'b) exp -> comp_op -> ('a, 'b) exp -> ('a, 'b) prim_fml
val icomp : ('a, 'b) iexp -> icomp_op -> ('a, 'b) iexp -> ('a, 'b) prim_fml
val lbinary : ('a, 'b) fml -> lbinop -> ('a, 'b) fml -> ('a, 'b) prim_fml

val quant :
  quant -> ('a, 'b) sim_binding list -> ('a, 'b) block -> ('a, 'b) prim_fml

val lunary : lunop -> ('a, 'b) fml -> ('a, 'b) prim_fml
val block : ('a, 'b) block -> ('a, 'b) prim_fml
val fite : ('a, 'b) fml -> ('a, 'b) fml -> ('a, 'b) fml -> ('a, 'b) prim_fml
val let_ : ('a, 'b) binding list -> ('a, 'b) block -> ('a, 'b) prim_fml
val all : quant
val some : quant
val no_ : quant
val lone : quant
val one : quant
val and_ : lbinop
val or_ : lbinop
val impl : lbinop
val iff : lbinop
val until : lbinop
val releases : lbinop
val triggered : lbinop
val since : lbinop
val not_ : lunop
val eventually : lunop
val always : lunop
val once : lunop
val next : lunop
val historically : lunop
val previous : lunop
val num : int -> ('a, 'b) prim_iexp
val none : ('a, 'b) prim_exp
val univ : ('a, 'b) prim_exp
val iden : ('a, 'b) prim_exp
val ident : 'a -> ('b, 'a) prim_exp
val runary : runop -> ('a, 'b) exp -> ('a, 'b) prim_exp
val rbinary : ('a, 'b) exp -> rbinop -> ('a, 'b) exp -> ('a, 'b) prim_exp
val rite : ('a, 'b) fml -> ('a, 'b) exp -> ('a, 'b) exp -> ('a, 'b) prim_exp
val boxjoin : ('a, 'b) exp -> ('a, 'b) exp list -> ('a, 'b) prim_exp
val compr : ('a, 'b) sim_binding list -> ('a, 'b) block -> ('a, 'b) prim_exp
val prime : ('a, 'b) exp -> ('a, 'b) prim_exp
val big_int : ('a, 'b) iexp -> ('a, 'b) prim_exp
val in_ : comp_op
val not_in : comp_op
val req : comp_op
val rneq : comp_op
val ieq : icomp_op
val ineq : icomp_op
val lt : icomp_op
val lte : icomp_op
val gt : icomp_op
val gte : icomp_op
val rone : rqualify
val rsome : rqualify
val rlone : rqualify
val rno : rqualify
val transpose : runop
val tclos : runop
val rtclos : runop
val union : rbinop
val inter : rbinop
val over : rbinop
val lproj : rbinop
val rproj : rbinop
val prod : rbinop
val diff : rbinop
val join : rbinop
val card : ('a, 'b) exp -> ('a, 'b) prim_iexp
val iunary : iunop -> ('a, 'b) iexp -> ('a, 'b) prim_iexp
val ibinary : ('a, 'b) iexp -> ibinop -> ('a, 'b) iexp -> ('a, 'b) prim_iexp
val neg : iunop
val add : ibinop
val sub : ibinop
val mul : ibinop
val div : ibinop
val rem : ibinop
val lshift : ibinop
val zershift : ibinop
val sershift : ibinop
val small_int : ('a, 'b) exp -> ('a, 'b) prim_iexp
val sum : ('a, 'b) binding list -> ('a, 'b) iexp -> ('a, 'b) prim_iexp
val fml : Location.t -> ('a, 'b) prim_fml -> ('a, 'b) fml
val exp : int option -> Location.t -> ('a, 'b) prim_exp -> ('a, 'b) exp
val iexp : Location.t -> ('a, 'b) prim_iexp -> ('a, 'b) iexp
val run : ('a, 'b) block -> bool option -> ('a, 'b) t
val get_expected : ('v, 'i) t -> bool option
val kwd_styled : 'a Fmtc.t -> 'a Fmtc.t

val pp :
  'a Fmtc.t ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) t ->
  unit

val pp_fml :
  'a Fmtc.t -> (Format.formatter -> 'b -> unit) -> ('a, 'b) fml Fmtc.t

val pp_prim_fml :
  'a Fmtc.t ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) prim_fml ->
  unit

val pp_block :
  'a Fmtc.t -> (Format.formatter -> 'b -> unit) -> ('a, 'b) block Fmtc.t

val pp_rqualify : Format.formatter -> rqualify -> unit
val pp_comp_op : Format.formatter -> comp_op -> unit
val pp_icomp_op : Format.formatter -> icomp_op -> unit
val pp_lunop : Format.formatter -> lunop -> unit
val pp_lbinop : Format.formatter -> lbinop -> unit
val pp_quant : Format.formatter -> quant -> unit

val pp_binding :
  sep:unit Fmtc.t ->
  'a Fmtc.t ->
  (Format.formatter -> 'b -> unit) ->
  ('a, 'b) binding Fmtc.t

val pp_sim_binding :
  'a Fmtc.t -> (Format.formatter -> 'b -> unit) -> ('a, 'b) sim_binding Fmtc.t

val pp_exp :
  ?show_arity:disj ->
  'a Fmtc.t ->
  (Format.formatter -> 'b -> unit) ->
  ('a, 'b) exp Fmtc.t

val pp_prim_exp :
  'a Fmtc.t ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) prim_exp ->
  unit

val pp_runop : Format.formatter -> runop -> unit
val pp_rbinop : Format.formatter -> rbinop -> unit

val pp_iexp :
  'a Fmtc.t ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) iexp ->
  unit

val pp_prim_iexp :
  'a Fmtc.t ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) prim_iexp ->
  unit

val pp_iunop : Format.formatter -> iunop -> unit
val pp_ibinop : Format.formatter -> ibinop -> unit
