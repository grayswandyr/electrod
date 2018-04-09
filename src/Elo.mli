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

type ('fml, 'exp, 'iexp) ofml = private
    True
  | False
  | RComp of 'exp * comp_op * 'exp
  | IComp of 'iexp * icomp_op * 'iexp
  | LUn of lunop * 'fml
  | LBin of 'fml * lbinop * 'fml
  | Quant of quant * (bool * int * 'exp) * 'fml list
  | FIte of 'fml * 'fml * 'fml
  | Block of 'fml list
and quant = private All | Some_ | No
and lbinop = private And | Or | Imp | Iff | U | R | S
and lunop = private F | G | Not | O | X | H | P
and comp_op = private In | NotIn | REq | RNEq
and icomp_op = private IEq | INEq | Lt | Lte | Gt | Gte
and ('fml, 'exp, 'iexp) oexp = {
  prim_exp : ('fml, 'exp, 'iexp) prim_oexp;
  arity : int;
}
and ('fml, 'exp, 'iexp) prim_oexp = private
    None_
  | Univ
  | Iden
  | Var of int
  | Name of Name.t
  | RUn of runop * 'exp
  | RBin of 'exp * rbinop * 'exp
  | RIte of 'fml * 'exp * 'exp
  | Compr of (bool * int * 'exp) list * 'fml list
  | Prime of 'exp
and runop = private Transpose | TClos | RTClos
and rbinop = private Union | Inter | Over | LProj | RProj | Prod | Diff | Join
and ('fml, 'exp, 'iexp) oiexp = private
    Num of int
  | Card of 'exp
  | IUn of iunop * 'iexp
  | IBin of 'iexp * ibinop * 'iexp
and iunop = private Neg
and ibinop = private Add | Sub


type goal = private Run of fml list [@@unboxed]

and fml = private Fml of (fml, exp, iexp) ofml Hashcons.hash_consed [@@unboxed]

and prim_exp = (fml, exp, iexp) prim_oexp

and exp = private Exp of (fml, exp, iexp) oexp Hashcons.hash_consed [@@unboxed]

and iexp = private Iexp of (fml, exp, iexp) oiexp Hashcons.hash_consed [@@unboxed]


class ['c] map :
  object ('c)
    constraint 'c =
      < visit_'exp : 'd -> exp -> exp; visit_'fml : 'd -> fml -> fml;
      visit_'iexp : 'd -> iexp -> iexp; visit_Add : 'd -> ibinop;
      visit_All : 'd -> quant; visit_And : 'd -> lbinop;
      visit_Block : 'd -> fml list -> (fml, exp, iexp) ofml;
      visit_Card : 'd -> exp -> (fml, exp, iexp) oiexp;
      visit_Compr : 'd ->
      (bool * int * exp) list ->
      fml list -> (fml, exp, iexp) prim_oexp;
      visit_Diff : 'd -> rbinop; visit_F : 'd -> lunop;
      visit_FIte : 'd -> fml -> fml -> fml -> (fml, exp, iexp) ofml;
      visit_False : 'd -> (fml, exp, iexp) ofml; visit_G : 'd -> lunop;
      visit_Gt : 'd -> icomp_op; visit_Gte : 'd -> icomp_op;
      visit_H : 'd -> lunop;
      visit_IBin : 'd -> iexp -> ibinop -> iexp -> (fml, exp, iexp) oiexp;
      visit_IComp : 'd ->
      iexp -> icomp_op -> iexp -> (fml, exp, iexp) ofml;
      visit_IEq : 'd -> icomp_op; visit_INEq : 'd -> icomp_op;
      visit_IUn : 'd -> iunop -> iexp -> (fml, exp, iexp) oiexp;
      visit_Iden : 'd -> (fml, exp, iexp) prim_oexp;
      visit_Iff : 'd -> lbinop; visit_Imp : 'd -> lbinop;
      visit_In : 'd -> comp_op; visit_Inter : 'd -> rbinop;
      visit_Join : 'd -> rbinop;
      visit_LBin : 'd -> fml -> lbinop -> fml -> (fml, exp, iexp) ofml;
      visit_LProj : 'd -> rbinop;
      visit_LUn : 'd -> lunop -> fml -> (fml, exp, iexp) ofml;
      visit_Lt : 'd -> icomp_op; visit_Lte : 'd -> icomp_op;
      visit_Name : 'd -> Name.t -> (fml, exp, iexp) prim_oexp;
      visit_Neg : 'd -> iunop; visit_No : 'd -> quant;
      visit_None_ : 'd -> (fml, exp, iexp) prim_oexp;
      visit_Not : 'd -> lunop; visit_NotIn : 'd -> comp_op;
      visit_Num : 'd -> int -> (fml, exp, iexp) oiexp;
      visit_O : 'd -> lunop; visit_Or : 'd -> lbinop;
      visit_Over : 'd -> rbinop; visit_P : 'd -> lunop;
      visit_Prime : 'd -> exp -> (fml, exp, iexp) prim_oexp;
      visit_Prod : 'd -> rbinop;
      visit_Quant : 'd ->
      quant ->
      bool * int * exp -> fml list -> (fml, exp, iexp) ofml;
      visit_R : 'd -> lbinop;
      visit_RBin : 'd ->
      exp -> rbinop -> exp -> (fml, exp, iexp) prim_oexp;
      visit_RComp : 'd -> exp -> comp_op -> exp -> (fml, exp, iexp) ofml;
      visit_REq : 'd -> comp_op;
      visit_RIte : 'd -> fml -> exp -> exp -> (fml, exp, iexp) prim_oexp;
      visit_RNEq : 'd -> comp_op; visit_RProj : 'd -> rbinop;
      visit_RTClos : 'd -> runop;
      visit_RUn : 'd -> runop -> exp -> (fml, exp, iexp) prim_oexp;
      visit_S : 'd -> lbinop; visit_Some_ : 'd -> quant;
      visit_Sub : 'd -> ibinop; visit_TClos : 'd -> runop;
      visit_Transpose : 'd -> runop;
      visit_True : 'd -> (fml, exp, iexp) ofml; visit_U : 'd -> lbinop;
      visit_Union : 'd -> rbinop;
      visit_Univ : 'd -> (fml, exp, iexp) prim_oexp;
      visit_Var : 'd -> int -> (fml, exp, iexp) prim_oexp;
      visit_X : 'd -> lunop; visit_comp_op : 'd -> comp_op -> comp_op;
      visit_exp : 'd -> exp -> exp; visit_fml : 'd -> fml -> fml;
      visit_ibinop : 'd -> ibinop -> ibinop;
      visit_icomp_op : 'd -> icomp_op -> icomp_op;
      visit_iexp : 'd -> iexp -> iexp;
      visit_iunop : 'd -> iunop -> iunop;
      visit_lbinop : 'd -> lbinop -> lbinop;
      visit_lunop : 'd -> lunop -> lunop;
      visit_oexp : 'd -> (fml, exp, iexp) oexp -> (fml, exp, iexp) oexp;
      visit_ofml : 'd -> (fml, exp, iexp) ofml -> (fml, exp, iexp) ofml;
      visit_oiexp : 'd ->
      (fml, exp, iexp) oiexp -> (fml, exp, iexp) oiexp;
      visit_prim_oexp : 'd ->
      (fml, exp, iexp) prim_oexp ->
      (fml, exp, iexp) prim_oexp;
      visit_quant : 'd -> quant -> quant;
      visit_rbinop : 'd -> rbinop -> rbinop;
      visit_runop : 'd -> runop -> runop; .. >
    method visit_'exp : 'd -> exp -> exp
    method visit_'fml : 'd -> fml -> fml
    method visit_'iexp : 'd -> iexp -> iexp
    method visit_Add : 'd -> ibinop
    method visit_All : 'd -> quant
    method visit_And : 'd -> lbinop
    method visit_Block : 'd -> fml list -> (fml, exp, iexp) ofml
    method visit_Card : 'd -> exp -> (fml, exp, iexp) oiexp
    method visit_Compr :
      'd ->
      (bool * int * exp) list -> fml list -> (fml, exp, iexp) prim_oexp
    method visit_Diff : 'd -> rbinop
    method visit_F : 'd -> lunop
    method visit_FIte : 'd -> fml -> fml -> fml -> (fml, exp, iexp) ofml
    method visit_False : 'd -> (fml, exp, iexp) ofml
    method visit_G : 'd -> lunop
    method visit_Gt : 'd -> icomp_op
    method visit_Gte : 'd -> icomp_op
    method visit_H : 'd -> lunop
    method visit_IBin :
      'd -> iexp -> ibinop -> iexp -> (fml, exp, iexp) oiexp
    method visit_IComp :
      'd -> iexp -> icomp_op -> iexp -> (fml, exp, iexp) ofml
    method visit_IEq : 'd -> icomp_op
    method visit_INEq : 'd -> icomp_op
    method visit_IUn : 'd -> iunop -> iexp -> (fml, exp, iexp) oiexp
    method visit_Iden : 'd -> (fml, exp, iexp) prim_oexp
    method visit_Iff : 'd -> lbinop
    method visit_Imp : 'd -> lbinop
    method visit_In : 'd -> comp_op
    method visit_Inter : 'd -> rbinop
    method visit_Join : 'd -> rbinop
    method visit_LBin : 'd -> fml -> lbinop -> fml -> (fml, exp, iexp) ofml
    method visit_LProj : 'd -> rbinop
    method visit_LUn : 'd -> lunop -> fml -> (fml, exp, iexp) ofml
    method visit_Lt : 'd -> icomp_op
    method visit_Lte : 'd -> icomp_op
    method visit_Name : 'd -> Name.t -> (fml, exp, iexp) prim_oexp
    method visit_Neg : 'd -> iunop
    method visit_No : 'd -> quant
    method visit_None_ : 'd -> (fml, exp, iexp) prim_oexp
    method visit_Not : 'd -> lunop
    method visit_NotIn : 'd -> comp_op
    method visit_Num : 'd -> int -> (fml, exp, iexp) oiexp
    method visit_O : 'd -> lunop
    method visit_Or : 'd -> lbinop
    method visit_Over : 'd -> rbinop
    method visit_P : 'd -> lunop
    method visit_Prime : 'd -> exp -> (fml, exp, iexp) prim_oexp
    method visit_Prod : 'd -> rbinop
    method visit_Quant :
      'd -> quant -> bool * int * exp -> fml list -> (fml, exp, iexp) ofml
    method visit_R : 'd -> lbinop
    method visit_RBin :
      'd -> exp -> rbinop -> exp -> (fml, exp, iexp) prim_oexp
    method visit_RComp :
      'd -> exp -> comp_op -> exp -> (fml, exp, iexp) ofml
    method visit_REq : 'd -> comp_op
    method visit_RIte :
      'd -> fml -> exp -> exp -> (fml, exp, iexp) prim_oexp
    method visit_RNEq : 'd -> comp_op
    method visit_RProj : 'd -> rbinop
    method visit_RTClos : 'd -> runop
    method visit_RUn : 'd -> runop -> exp -> (fml, exp, iexp) prim_oexp
    method visit_S : 'd -> lbinop
    method visit_Some_ : 'd -> quant
    method visit_Sub : 'd -> ibinop
    method visit_TClos : 'd -> runop
    method visit_Transpose : 'd -> runop
    method visit_True : 'd -> (fml, exp, iexp) ofml
    method visit_U : 'd -> lbinop
    method visit_Union : 'd -> rbinop
    method visit_Univ : 'd -> (fml, exp, iexp) prim_oexp
    method visit_Var : 'd -> int -> (fml, exp, iexp) prim_oexp
    method visit_X : 'd -> lunop
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_comp_op : 'd -> comp_op -> comp_op
    method visit_exp : 'd -> exp -> exp
    method private visit_float : 'env. 'env -> float -> float
    method visit_fml : 'd -> fml -> fml
    method visit_ibinop : 'd -> ibinop -> ibinop
    method visit_icomp_op : 'd -> icomp_op -> icomp_op
    method visit_iexp : 'd -> iexp -> iexp
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
    method visit_oexp :
      'd -> (fml, exp, iexp) oexp -> (fml, exp, iexp) oexp
    method visit_ofml :
      'd -> (fml, exp, iexp) ofml -> (fml, exp, iexp) ofml
    method visit_oiexp :
      'd -> (fml, exp, iexp) oiexp -> (fml, exp, iexp) oiexp
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_prim_oexp :
      'd -> (fml, exp, iexp) prim_oexp -> (fml, exp, iexp) prim_oexp
    method visit_quant : 'd -> quant -> quant
    method visit_rbinop : 'd -> rbinop -> rbinop
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b) ->
      ('env -> 'e -> 'f) -> 'env -> ('a, 'e) result -> ('b, 'f) result
    method visit_runop : 'd -> runop -> runop
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['c] fold :
  object ('c)
    constraint 'c =
      < build_Add : 'd -> 'g; build_All : 'd -> 'h; build_And : 'd -> 'i;
      build_Block : 'd -> 'j list -> 'j; build_Card : 'd -> 'k -> 'l;
      build_Compr : 'd -> (bool * int * 'k) list -> 'j list -> 'm;
      build_Diff : 'd -> 'n; build_F : 'd -> 'o;
      build_FIte : 'd -> 'j -> 'j -> 'j -> 'j; build_False : 'd -> 'j;
      build_G : 'd -> 'o; build_Gt : 'd -> 'p; build_Gte : 'd -> 'p;
      build_H : 'd -> 'o; build_IBin : 'd -> 'l -> 'g -> 'l -> 'l;
      build_IComp : 'd -> 'l -> 'p -> 'l -> 'j; build_IEq : 'd -> 'p;
      build_INEq : 'd -> 'p; build_IUn : 'd -> 'q -> 'l -> 'l;
      build_Iden : 'd -> 'm; build_Iff : 'd -> 'i; build_Imp : 
                                                     'd -> 'i; build_In : 'd -> 'r; build_Inter : 'd -> 'n;
      build_Join : 'd -> 'n; build_LBin : 'd -> 'j -> 'i -> 'j -> 'j;
      build_LProj : 'd -> 'n; build_LUn : 'd -> 'o -> 'j -> 'j;
      build_Lt : 'd -> 'p; build_Lte : 'd -> 'p;
      build_Name : 'd -> Name.t -> 'm; build_Neg : 'd -> 'q;
      build_No : 'd -> 'h; build_None_ : 'd -> 'm; build_Not : 
                                                     'd -> 'o; build_NotIn : 'd -> 'r; build_Num : 'd -> int -> 'l;
      build_O : 'd -> 'o; build_Or : 'd -> 'i; build_Over : 'd -> 'n;
      build_P : 'd -> 'o; build_Prime : 'd -> 'k -> 'm;
      build_Prod : 'd -> 'n;
      build_Quant : 'd -> 'h -> bool * int * 'k -> 'j list -> 'j;
      build_R : 'd -> 'i; build_RBin : 'd -> 'k -> 'n -> 'k -> 'm;
      build_RComp : 'd -> 'k -> 'r -> 'k -> 'j; build_REq : 'd -> 'r;
      build_RIte : 'd -> 'j -> 'k -> 'k -> 'm; build_RNEq : 'd -> 'r;
      build_RProj : 'd -> 'n; build_RTClos : 'd -> 's;
      build_RUn : 'd -> 's -> 'k -> 'm; build_S : 'd -> 'i;
      build_Some_ : 'd -> 'h; build_Sub : 'd -> 'g;
      build_TClos : 'd -> 's; build_Transpose : 'd -> 's;
      build_True : 'd -> 'j; build_U : 'd -> 'i; build_Union : 
                                                   'd -> 'n; build_Univ : 'd -> 'm; build_Var : 'd -> int -> 'm;
      build_X : 'd -> 'o; build_oexp : 'd -> 'm -> int -> 'k;
      visit_'exp : 'd -> exp -> 'k; visit_'fml : 'd -> fml -> 'j;
      visit_'iexp : 'd -> iexp -> 'l; visit_Add : 'd -> 'g;
      visit_All : 'd -> 'h; visit_And : 'd -> 'i;
      visit_Block : 'd -> fml list -> 'j; visit_Card : 'd -> exp -> 'l;
      visit_Compr : 'd -> (bool * int * exp) list -> fml list -> 'm;
      visit_Diff : 'd -> 'n; visit_F : 'd -> 'o;
      visit_FIte : 'd -> fml -> fml -> fml -> 'j; visit_False : 
                                                    'd -> 'j; visit_G : 'd -> 'o; visit_Gt : 'd -> 'p;
      visit_Gte : 'd -> 'p; visit_H : 'd -> 'o;
      visit_IBin : 'd -> iexp -> ibinop -> iexp -> 'l;
      visit_IComp : 'd -> iexp -> icomp_op -> iexp -> 'j;
      visit_IEq : 'd -> 'p; visit_INEq : 'd -> 'p;
      visit_IUn : 'd -> iunop -> iexp -> 'l; visit_Iden : 'd -> 'm;
      visit_Iff : 'd -> 'i; visit_Imp : 'd -> 'i; visit_In : 'd -> 'r;
      visit_Inter : 'd -> 'n; visit_Join : 'd -> 'n;
      visit_LBin : 'd -> fml -> lbinop -> fml -> 'j;
      visit_LProj : 'd -> 'n; visit_LUn : 'd -> lunop -> fml -> 'j;
      visit_Lt : 'd -> 'p; visit_Lte : 'd -> 'p;
      visit_Name : 'd -> Name.t -> 'm; visit_Neg : 'd -> 'q;
      visit_No : 'd -> 'h; visit_None_ : 'd -> 'm; visit_Not : 
                                                     'd -> 'o; visit_NotIn : 'd -> 'r; visit_Num : 'd -> int -> 'l;
      visit_O : 'd -> 'o; visit_Or : 'd -> 'i; visit_Over : 'd -> 'n;
      visit_P : 'd -> 'o; visit_Prime : 'd -> exp -> 'm;
      visit_Prod : 'd -> 'n;
      visit_Quant : 'd -> quant -> bool * int * exp -> fml list -> 'j;
      visit_R : 'd -> 'i; visit_RBin : 'd -> exp -> rbinop -> exp -> 'm;
      visit_RComp : 'd -> exp -> comp_op -> exp -> 'j;
      visit_REq : 'd -> 'r; visit_RIte : 'd -> fml -> exp -> exp -> 'm;
      visit_RNEq : 'd -> 'r; visit_RProj : 'd -> 'n;
      visit_RTClos : 'd -> 's; visit_RUn : 'd -> runop -> exp -> 'm;
      visit_S : 'd -> 'i; visit_Some_ : 'd -> 'h; visit_Sub : 'd -> 'g;
      visit_TClos : 'd -> 's; visit_Transpose : 'd -> 's;
      visit_True : 'd -> 'j; visit_U : 'd -> 'i; visit_Union : 
                                                   'd -> 'n; visit_Univ : 'd -> 'm; visit_Var : 'd -> int -> 'm;
      visit_X : 'd -> 'o; visit_comp_op : 'd -> comp_op -> 'r;
      visit_exp : 'd -> exp -> 'k; visit_fml : 'd -> fml -> 'j;
      visit_ibinop : 'd -> ibinop -> 'g;
      visit_icomp_op : 'd -> icomp_op -> 'p;
      visit_iexp : 'd -> iexp -> 'l; visit_iunop : 'd -> iunop -> 'q;
      visit_lbinop : 'd -> lbinop -> 'i; visit_lunop : 'd -> lunop -> 'o;
      visit_oexp : 'd -> (fml, exp, iexp) oexp -> 'k;
      visit_ofml : 'd -> (fml, exp, iexp) ofml -> 'j;
      visit_oiexp : 'd -> (fml, exp, iexp) oiexp -> 'l;
      visit_prim_oexp : 'd -> (fml, exp, iexp) prim_oexp -> 'm;
      visit_quant : 'd -> quant -> 'h; visit_rbinop : 'd -> rbinop -> 'n;
      visit_runop : 'd -> runop -> 's; .. >
    method virtual build_Add : 'd -> 'g
    method virtual build_All : 'd -> 'h
    method virtual build_And : 'd -> 'i
    method virtual build_Block : 'd -> 'j list -> 'j
    method virtual build_Card : 'd -> 'k -> 'l
    method virtual build_Compr :
      'd -> (bool * int * 'k) list -> 'j list -> 'm
    method virtual build_Diff : 'd -> 'n
    method virtual build_F : 'd -> 'o
    method virtual build_FIte : 'd -> 'j -> 'j -> 'j -> 'j
    method virtual build_False : 'd -> 'j
    method virtual build_G : 'd -> 'o
    method virtual build_Gt : 'd -> 'p
    method virtual build_Gte : 'd -> 'p
    method virtual build_H : 'd -> 'o
    method virtual build_IBin : 'd -> 'l -> 'g -> 'l -> 'l
    method virtual build_IComp : 'd -> 'l -> 'p -> 'l -> 'j
    method virtual build_IEq : 'd -> 'p
    method virtual build_INEq : 'd -> 'p
    method virtual build_IUn : 'd -> 'q -> 'l -> 'l
    method virtual build_Iden : 'd -> 'm
    method virtual build_Iff : 'd -> 'i
    method virtual build_Imp : 'd -> 'i
    method virtual build_In : 'd -> 'r
    method virtual build_Inter : 'd -> 'n
    method virtual build_Join : 'd -> 'n
    method virtual build_LBin : 'd -> 'j -> 'i -> 'j -> 'j
    method virtual build_LProj : 'd -> 'n
    method virtual build_LUn : 'd -> 'o -> 'j -> 'j
    method virtual build_Lt : 'd -> 'p
    method virtual build_Lte : 'd -> 'p
    method virtual build_Name : 'd -> Name.t -> 'm
    method virtual build_Neg : 'd -> 'q
    method virtual build_No : 'd -> 'h
    method virtual build_None_ : 'd -> 'm
    method virtual build_Not : 'd -> 'o
    method virtual build_NotIn : 'd -> 'r
    method virtual build_Num : 'd -> int -> 'l
    method virtual build_O : 'd -> 'o
    method virtual build_Or : 'd -> 'i
    method virtual build_Over : 'd -> 'n
    method virtual build_P : 'd -> 'o
    method virtual build_Prime : 'd -> 'k -> 'm
    method virtual build_Prod : 'd -> 'n
    method virtual build_Quant :
      'd -> 'h -> bool * int * 'k -> 'j list -> 'j
    method virtual build_R : 'd -> 'i
    method virtual build_RBin : 'd -> 'k -> 'n -> 'k -> 'm
    method virtual build_RComp : 'd -> 'k -> 'r -> 'k -> 'j
    method virtual build_REq : 'd -> 'r
    method virtual build_RIte : 'd -> 'j -> 'k -> 'k -> 'm
    method virtual build_RNEq : 'd -> 'r
    method virtual build_RProj : 'd -> 'n
    method virtual build_RTClos : 'd -> 's
    method virtual build_RUn : 'd -> 's -> 'k -> 'm
    method virtual build_S : 'd -> 'i
    method virtual build_Some_ : 'd -> 'h
    method virtual build_Sub : 'd -> 'g
    method virtual build_TClos : 'd -> 's
    method virtual build_Transpose : 'd -> 's
    method virtual build_True : 'd -> 'j
    method virtual build_U : 'd -> 'i
    method virtual build_Union : 'd -> 'n
    method virtual build_Univ : 'd -> 'm
    method virtual build_Var : 'd -> int -> 'm
    method virtual build_X : 'd -> 'o
    method virtual build_oexp : 'd -> 'm -> int -> 'k
    method visit_'exp : 'd -> exp -> 'k
    method visit_'fml : 'd -> fml -> 'j
    method visit_'iexp : 'd -> iexp -> 'l
    method visit_Add : 'd -> 'g
    method visit_All : 'd -> 'h
    method visit_And : 'd -> 'i
    method visit_Block : 'd -> fml list -> 'j
    method visit_Card : 'd -> exp -> 'l
    method visit_Compr : 'd -> (bool * int * exp) list -> fml list -> 'm
    method visit_Diff : 'd -> 'n
    method visit_F : 'd -> 'o
    method visit_FIte : 'd -> fml -> fml -> fml -> 'j
    method visit_False : 'd -> 'j
    method visit_G : 'd -> 'o
    method visit_Gt : 'd -> 'p
    method visit_Gte : 'd -> 'p
    method visit_H : 'd -> 'o
    method visit_IBin : 'd -> iexp -> ibinop -> iexp -> 'l
    method visit_IComp : 'd -> iexp -> icomp_op -> iexp -> 'j
    method visit_IEq : 'd -> 'p
    method visit_INEq : 'd -> 'p
    method visit_IUn : 'd -> iunop -> iexp -> 'l
    method visit_Iden : 'd -> 'm
    method visit_Iff : 'd -> 'i
    method visit_Imp : 'd -> 'i
    method visit_In : 'd -> 'r
    method visit_Inter : 'd -> 'n
    method visit_Join : 'd -> 'n
    method visit_LBin : 'd -> fml -> lbinop -> fml -> 'j
    method visit_LProj : 'd -> 'n
    method visit_LUn : 'd -> lunop -> fml -> 'j
    method visit_Lt : 'd -> 'p
    method visit_Lte : 'd -> 'p
    method visit_Name : 'd -> Name.t -> 'm
    method visit_Neg : 'd -> 'q
    method visit_No : 'd -> 'h
    method visit_None_ : 'd -> 'm
    method visit_Not : 'd -> 'o
    method visit_NotIn : 'd -> 'r
    method visit_Num : 'd -> int -> 'l
    method visit_O : 'd -> 'o
    method visit_Or : 'd -> 'i
    method visit_Over : 'd -> 'n
    method visit_P : 'd -> 'o
    method visit_Prime : 'd -> exp -> 'm
    method visit_Prod : 'd -> 'n
    method visit_Quant : 'd -> quant -> bool * int * exp -> fml list -> 'j
    method visit_R : 'd -> 'i
    method visit_RBin : 'd -> exp -> rbinop -> exp -> 'm
    method visit_RComp : 'd -> exp -> comp_op -> exp -> 'j
    method visit_REq : 'd -> 'r
    method visit_RIte : 'd -> fml -> exp -> exp -> 'm
    method visit_RNEq : 'd -> 'r
    method visit_RProj : 'd -> 'n
    method visit_RTClos : 'd -> 's
    method visit_RUn : 'd -> runop -> exp -> 'm
    method visit_S : 'd -> 'i
    method visit_Some_ : 'd -> 'h
    method visit_Sub : 'd -> 'g
    method visit_TClos : 'd -> 's
    method visit_Transpose : 'd -> 's
    method visit_True : 'd -> 'j
    method visit_U : 'd -> 'i
    method visit_Union : 'd -> 'n
    method visit_Univ : 'd -> 'm
    method visit_Var : 'd -> int -> 'm
    method visit_X : 'd -> 'o
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_comp_op : 'd -> comp_op -> 'r
    method visit_exp : 'd -> exp -> 'k
    method private visit_float : 'env. 'env -> float -> float
    method visit_fml : 'd -> fml -> 'j
    method visit_ibinop : 'd -> ibinop -> 'g
    method visit_icomp_op : 'd -> icomp_op -> 'p
    method visit_iexp : 'd -> iexp -> 'l
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_iunop : 'd -> iunop -> 'q
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t
    method visit_lbinop : 'd -> lbinop -> 'i
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_lunop : 'd -> lunop -> 'o
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_oexp : 'd -> (fml, exp, iexp) oexp -> 'k
    method visit_ofml : 'd -> (fml, exp, iexp) ofml -> 'j
    method visit_oiexp : 'd -> (fml, exp, iexp) oiexp -> 'l
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_prim_oexp : 'd -> (fml, exp, iexp) prim_oexp -> 'm
    method visit_quant : 'd -> quant -> 'h
    method visit_rbinop : 'd -> rbinop -> 'n
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b) ->
      ('env -> 'e -> 'f) -> 'env -> ('a, 'e) result -> ('b, 'f) result
    method visit_runop : 'd -> runop -> 's
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end

type t = {
  file : string option;
  domain : Domain.t;
  instance : Instance.t;
  sym : Symmetry.t list;
  invariants : fml list;
  goal : goal;
  atom_renaming : (Atom.t, Atom.t) CCList.Assoc.t;
  name_renaming : (Name.t, Name.t) CCList.Assoc.t;
}

val make :
  string option ->
  Domain.t ->
  Instance.t ->
  Symmetry.t list ->
  fml list ->
  goal ->
  (Atom.t, Atom.t) CCList.Assoc.t -> (Name.t, Name.t) CCList.Assoc.t -> t
val arity : exp -> int
val run : fml list -> goal
val true_ : fml
val false_ : fml
val rcomp : exp -> comp_op -> exp -> fml
val icomp : iexp -> icomp_op -> iexp -> fml
val lbinary : fml -> lbinop -> fml -> fml
val sim_binding : bool -> int -> exp -> bool * int * exp
val quant : quant -> bool * int * exp -> fml list -> fml
val lunary : lunop -> fml -> fml
val block : fml list -> fml
val fite : fml -> fml -> fml -> fml
val all : quant
val some : quant
val no_ : quant
val and_ : lbinop
val or_ : lbinop
val impl : lbinop
val iff : lbinop
val until : lbinop
val releases : lbinop
val since : lbinop
val not_ : lunop
val sometime : lunop
val always : lunop
val once : lunop
val next : lunop
val historically : lunop
val previous : lunop
val none : exp
val univ : exp
val iden : exp
val var : ar:int -> int -> exp
val name : ar:int -> Name.t -> exp
val runary : ar:int -> runop -> exp -> exp
val rbinary : ar:int -> exp -> rbinop -> exp -> exp
val rite : ar:int -> fml -> exp -> exp -> exp
val compr : ar:int -> (bool * int * exp) list -> fml list -> exp
val prime : ar:int -> exp -> exp
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
val num : int -> iexp
val card : exp -> iexp
val iunary : iunop -> iexp -> iexp
val ibinary : iexp -> ibinop -> iexp -> iexp
val neg : iunop
val add : ibinop
val sub : ibinop
val kwd_styled : 'a Fmtc.t -> 'a Fmtc.t
val pp_comp_op : Format.formatter -> comp_op -> unit
val pp_icomp_op : Format.formatter -> icomp_op -> unit
val pp_lunop : Format.formatter -> lunop -> unit
val pp_lbinop : Format.formatter -> lbinop -> unit
val pp_quant : Format.formatter -> quant -> unit
val pp_runop : Format.formatter -> runop -> unit
val pp_rbinop : Format.formatter -> rbinop -> unit
val pp_iunop : Format.formatter -> iunop -> unit
val pp_ibinop : Format.formatter -> ibinop -> unit
val pp_var : Format.formatter -> int -> unit
val pp_osim_binding :
  int ->
  (int -> Format.formatter -> 'a -> unit) ->
  Format.formatter -> bool * int * 'a -> unit
val pp_oblock :
  'a -> ('a -> 'b Fmtc.t) -> Format.formatter -> 'b list -> unit
val pp_ofml :
  int ->
  (int -> 'a Fmtc.t) ->
  (int -> Format.formatter -> 'b -> unit) ->
  (int -> Format.formatter -> 'c -> unit) ->
  Format.formatter -> ('a, 'b, 'c) ofml -> unit
val pp_prim_oexp :
  int ->
  (int -> 'a Fmtc.t) ->
  (int -> Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b, 'c) prim_oexp -> unit
val pp_oiexp :
  'a ->
  ('a -> Format.formatter -> 'b -> unit) ->
  ('a -> Format.formatter -> 'c -> unit) ->
  Format.formatter -> ('d, 'b, 'c) oiexp -> unit
val pp_fml : int -> fml Fmtc.t
val pp_block : int -> Format.formatter -> fml list -> unit
val pp_iexp : int -> Format.formatter -> iexp -> unit
val pp_prim_exp :
  int -> Format.formatter -> (fml, exp, iexp) prim_oexp -> unit
val pp_exp : int -> Format.formatter -> exp -> unit
val pp_goal : Format.formatter -> goal -> unit
val pp : Format.formatter -> t -> unit
