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

type ('fml, 'exp, 'iexp) ofml = private
  | True
  | False
  | RComp of 'exp * comp_op * 'exp
  | IComp of 'iexp * icomp_op * 'iexp
  | LUn of lunop * 'fml
  | LBin of 'fml * lbinop * 'fml
  | Quant of quant * (bool * int * 'exp) * 'fml list
  | FIte of 'fml * 'fml * 'fml
  | Block of 'fml list

and quant = private All | Some_ | No
and lbinop = private And | Or | Imp | Iff | U | R | S | T
and lunop = private F | G | Not | O | X | H | P
and comp_op = private In | NotIn | REq | RNEq
and icomp_op = private IEq | INEq | Lt | Lte | Gt | Gte

and ('fml, 'exp, 'iexp) oexp = {
  prim_exp : ('fml, 'exp, 'iexp) prim_oexp;
  arity : int;
}

and ('fml, 'exp, 'iexp) prim_oexp = private
  | None_
  | Univ
  | Iden
  | Var of int
  | Name of Name.t
  | RUn of runop * 'exp
  | RBin of 'exp * rbinop * 'exp
  | RIte of 'fml * 'exp * 'exp
  | Compr of (bool * int * 'exp) list * 'fml list
  | Prime of 'exp
  | Big_int of 'iexp

and runop = private Transpose | TClos | RTClos
and rbinop = private Union | Inter | Over | LProj | RProj | Prod | Diff | Join

and ('fml, 'exp, 'iexp) oiexp = private
  | Num of int
  | Card of 'exp
  | IUn of iunop * 'iexp
  | IBin of 'iexp * ibinop * 'iexp
  | Small_int of 'exp
  | Sum of 'exp * 'iexp
  | AIte of 'fml * 'iexp * 'iexp

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

type goal = private Run of (fml list * bool option) [@@unboxed]

and fml = private Fml of (fml, exp, iexp) ofml Hashcons.hash_consed
[@@unboxed]

and prim_exp = (fml, exp, iexp) prim_oexp

and exp = private Exp of (fml, exp, iexp) oexp Hashcons.hash_consed
[@@unboxed]

and iexp = private Iexp of (fml, exp, iexp) oiexp Hashcons.hash_consed
[@@unboxed]

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
  (Atom.t, Atom.t) CCList.Assoc.t ->
  (Name.t, Name.t) CCList.Assoc.t ->
  t

val arity : exp -> int
val run : fml list -> bool option -> goal
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
val triggered : lbinop
val not_ : lunop
val eventually : lunop
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
val big_int : iexp -> exp
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
val ifthenelse_arith : fml -> iexp -> iexp -> iexp
val neg : iunop
val add : ibinop
val sub : ibinop
val mul : ibinop
val div : ibinop
val rem : ibinop
val lshift : ibinop
val sershift : ibinop
val zershift : ibinop
val small_int : exp -> iexp
val sum : exp -> iexp -> iexp
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
val pp_sim_binding : int -> Format.formatter -> bool * int * exp -> unit
val pp_sim_bindings : int -> Format.formatter -> (bool * int * exp) list -> unit
val pp_oblock : 'a -> ('a -> 'b Fmtc.t) -> Format.formatter -> 'b list -> unit

val pp_ofml :
  int ->
  (int -> 'a Fmtc.t) ->
  (int -> Format.formatter -> 'b -> unit) ->
  (int -> Format.formatter -> 'c -> unit) ->
  Format.formatter ->
  ('a, 'b, 'c) ofml ->
  unit

val pp_prim_oexp :
  int ->
  (int -> 'a Fmtc.t) ->
  (int -> Format.formatter -> 'b -> unit) ->
  (int -> Format.formatter -> 'c -> unit) ->
  Format.formatter ->
  ('a, 'b, 'c) prim_oexp ->
  unit

val pp_oiexp :
  int ->
  (int -> Format.formatter -> 'a -> unit) ->
  (int -> Format.formatter -> 'b -> unit) ->
  (int -> Format.formatter -> 'c -> unit) ->
  Format.formatter ->
  ('a, 'b, 'c) oiexp ->
  unit

val pp_fml : int -> fml Fmtc.t
val pp_block : int -> Format.formatter -> fml list -> unit
val pp_iexp : int -> Format.formatter -> iexp -> unit
val pp_prim_exp : int -> Format.formatter -> (fml, exp, iexp) prim_oexp -> unit
val pp_exp : int -> Format.formatter -> exp -> unit
val pp_goal : Format.formatter -> goal -> unit
val pp : Format.formatter -> t -> unit

val pp_fml_stats : Format.formatter -> int -> unit
(** Prints the number of times every subformula is referenced 
    (if number > [inf]). *)

class ['c] map :
  object ('c)
    constraint
    'c = < visit_'exp : 'd -> exp -> exp
         ; visit_'fml : 'd -> fml -> fml
         ; visit_'iexp : 'd -> iexp -> iexp
         ; visit_AIte : 'd -> fml -> iexp -> iexp -> (fml, exp, iexp) oiexp
         ; visit_Add : 'd -> ibinop
         ; visit_All : 'd -> quant
         ; visit_And : 'd -> lbinop
         ; visit_Big_int : 'd -> iexp -> (fml, exp, iexp) prim_oexp
         ; visit_Block : 'd -> fml list -> (fml, exp, iexp) ofml
         ; visit_Card : 'd -> exp -> (fml, exp, iexp) oiexp
         ; visit_Compr :
             'd ->
             (bool * int * exp) list ->
             fml list ->
             (fml, exp, iexp) prim_oexp
         ; visit_Diff : 'd -> rbinop
         ; visit_Div : 'd -> ibinop
         ; visit_F : 'd -> lunop
         ; visit_FIte : 'd -> fml -> fml -> fml -> (fml, exp, iexp) ofml
         ; visit_False : 'd -> (fml, exp, iexp) ofml
         ; visit_G : 'd -> lunop
         ; visit_Gt : 'd -> icomp_op
         ; visit_Gte : 'd -> icomp_op
         ; visit_H : 'd -> lunop
         ; visit_IBin : 'd -> iexp -> ibinop -> iexp -> (fml, exp, iexp) oiexp
         ; visit_IComp : 'd -> iexp -> icomp_op -> iexp -> (fml, exp, iexp) ofml
         ; visit_IEq : 'd -> icomp_op
         ; visit_INEq : 'd -> icomp_op
         ; visit_IUn : 'd -> iunop -> iexp -> (fml, exp, iexp) oiexp
         ; visit_Iden : 'd -> (fml, exp, iexp) prim_oexp
         ; visit_Iff : 'd -> lbinop
         ; visit_Imp : 'd -> lbinop
         ; visit_In : 'd -> comp_op
         ; visit_Inter : 'd -> rbinop
         ; visit_Join : 'd -> rbinop
         ; visit_LBin : 'd -> fml -> lbinop -> fml -> (fml, exp, iexp) ofml
         ; visit_LProj : 'd -> rbinop
         ; visit_LUn : 'd -> lunop -> fml -> (fml, exp, iexp) ofml
         ; visit_Lshift : 'd -> ibinop
         ; visit_Lt : 'd -> icomp_op
         ; visit_Lte : 'd -> icomp_op
         ; visit_Mul : 'd -> ibinop
         ; visit_Name : 'd -> Name.t -> (fml, exp, iexp) prim_oexp
         ; visit_Neg : 'd -> iunop
         ; visit_No : 'd -> quant
         ; visit_None_ : 'd -> (fml, exp, iexp) prim_oexp
         ; visit_Not : 'd -> lunop
         ; visit_NotIn : 'd -> comp_op
         ; visit_Num : 'd -> int -> (fml, exp, iexp) oiexp
         ; visit_O : 'd -> lunop
         ; visit_Or : 'd -> lbinop
         ; visit_Over : 'd -> rbinop
         ; visit_P : 'd -> lunop
         ; visit_Prime : 'd -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_Prod : 'd -> rbinop
         ; visit_Quant :
             'd ->
             quant ->
             bool * int * exp ->
             fml list ->
             (fml, exp, iexp) ofml
         ; visit_R : 'd -> lbinop
         ; visit_RBin : 'd -> exp -> rbinop -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_RComp : 'd -> exp -> comp_op -> exp -> (fml, exp, iexp) ofml
         ; visit_REq : 'd -> comp_op
         ; visit_RIte : 'd -> fml -> exp -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_RNEq : 'd -> comp_op
         ; visit_RProj : 'd -> rbinop
         ; visit_RTClos : 'd -> runop
         ; visit_RUn : 'd -> runop -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_Rem : 'd -> ibinop
         ; visit_S : 'd -> lbinop
         ; visit_Sershift : 'd -> ibinop
         ; visit_Small_int : 'd -> exp -> (fml, exp, iexp) oiexp
         ; visit_Some_ : 'd -> quant
         ; visit_Sub : 'd -> ibinop
         ; visit_Sum : 'd -> exp -> iexp -> (fml, exp, iexp) oiexp
         ; visit_T : 'd -> lbinop
         ; visit_TClos : 'd -> runop
         ; visit_Transpose : 'd -> runop
         ; visit_True : 'd -> (fml, exp, iexp) ofml
         ; visit_U : 'd -> lbinop
         ; visit_Union : 'd -> rbinop
         ; visit_Univ : 'd -> (fml, exp, iexp) prim_oexp
         ; visit_Var : 'd -> int -> (fml, exp, iexp) prim_oexp
         ; visit_X : 'd -> lunop
         ; visit_Zershift : 'd -> ibinop
         ; visit_comp_op : 'd -> comp_op -> comp_op
         ; visit_exp : 'd -> exp -> exp
         ; visit_fml : 'd -> fml -> fml
         ; visit_ibinop : 'd -> ibinop -> ibinop
         ; visit_icomp_op : 'd -> icomp_op -> icomp_op
         ; visit_iexp : 'd -> iexp -> iexp
         ; visit_iunop : 'd -> iunop -> iunop
         ; visit_lbinop : 'd -> lbinop -> lbinop
         ; visit_lunop : 'd -> lunop -> lunop
         ; visit_oexp : 'd -> (fml, exp, iexp) oexp -> (fml, exp, iexp) oexp
         ; visit_ofml : 'd -> (fml, exp, iexp) ofml -> (fml, exp, iexp) ofml
         ; visit_oiexp : 'd -> (fml, exp, iexp) oiexp -> (fml, exp, iexp) oiexp
         ; visit_prim_oexp :
             'd -> (fml, exp, iexp) prim_oexp -> (fml, exp, iexp) prim_oexp
         ; visit_quant : 'd -> quant -> quant
         ; visit_rbinop : 'd -> rbinop -> rbinop
         ; visit_runop : 'd -> runop -> runop
         ; .. >

    method visit_'exp : 'd -> exp -> exp
    method visit_'fml : 'd -> fml -> fml
    method visit_'iexp : 'd -> iexp -> iexp
    method visit_AIte : 'd -> fml -> iexp -> iexp -> (fml, exp, iexp) oiexp
    method visit_Add : 'd -> ibinop
    method visit_All : 'd -> quant
    method visit_And : 'd -> lbinop
    method visit_Big_int : 'd -> iexp -> (fml, exp, iexp) prim_oexp
    method visit_Block : 'd -> fml list -> (fml, exp, iexp) ofml
    method visit_Card : 'd -> exp -> (fml, exp, iexp) oiexp

    method visit_Compr :
      'd -> (bool * int * exp) list -> fml list -> (fml, exp, iexp) prim_oexp

    method visit_Diff : 'd -> rbinop
    method visit_Div : 'd -> ibinop
    method visit_F : 'd -> lunop
    method visit_FIte : 'd -> fml -> fml -> fml -> (fml, exp, iexp) ofml
    method visit_False : 'd -> (fml, exp, iexp) ofml
    method visit_G : 'd -> lunop
    method visit_Gt : 'd -> icomp_op
    method visit_Gte : 'd -> icomp_op
    method visit_H : 'd -> lunop
    method visit_IBin : 'd -> iexp -> ibinop -> iexp -> (fml, exp, iexp) oiexp
    method visit_IComp : 'd -> iexp -> icomp_op -> iexp -> (fml, exp, iexp) ofml
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
    method visit_Lshift : 'd -> ibinop
    method visit_Lt : 'd -> icomp_op
    method visit_Lte : 'd -> icomp_op
    method visit_Mul : 'd -> ibinop
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
    method visit_RBin : 'd -> exp -> rbinop -> exp -> (fml, exp, iexp) prim_oexp
    method visit_RComp : 'd -> exp -> comp_op -> exp -> (fml, exp, iexp) ofml
    method visit_REq : 'd -> comp_op
    method visit_RIte : 'd -> fml -> exp -> exp -> (fml, exp, iexp) prim_oexp
    method visit_RNEq : 'd -> comp_op
    method visit_RProj : 'd -> rbinop
    method visit_RTClos : 'd -> runop
    method visit_RUn : 'd -> runop -> exp -> (fml, exp, iexp) prim_oexp
    method visit_Rem : 'd -> ibinop
    method visit_S : 'd -> lbinop
    method visit_Sershift : 'd -> ibinop
    method visit_Small_int : 'd -> exp -> (fml, exp, iexp) oiexp
    method visit_Some_ : 'd -> quant
    method visit_Sub : 'd -> ibinop
    method visit_Sum : 'd -> exp -> iexp -> (fml, exp, iexp) oiexp
    method visit_T : 'd -> lbinop
    method visit_TClos : 'd -> runop
    method visit_Transpose : 'd -> runop
    method visit_True : 'd -> (fml, exp, iexp) ofml
    method visit_U : 'd -> lbinop
    method visit_Union : 'd -> rbinop
    method visit_Univ : 'd -> (fml, exp, iexp) prim_oexp
    method visit_Var : 'd -> int -> (fml, exp, iexp) prim_oexp
    method visit_X : 'd -> lunop
    method visit_Zershift : 'd -> ibinop

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
    method visit_oexp : 'd -> (fml, exp, iexp) oexp -> (fml, exp, iexp) oexp
    method visit_ofml : 'd -> (fml, exp, iexp) ofml -> (fml, exp, iexp) ofml
    method visit_oiexp : 'd -> (fml, exp, iexp) oiexp -> (fml, exp, iexp) oiexp

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
      ('env -> 'e -> 'f) ->
      'env ->
      ('a, 'e) result ->
      ('b, 'f) result

    method visit_runop : 'd -> runop -> runop
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end

class virtual ['c] fold :
  object ('c)
    constraint
    'c = < build_AIte : 'd -> 'g -> 'h -> 'h -> 'h
         ; build_Add : 'd -> 'i
         ; build_All : 'd -> 'j
         ; build_And : 'd -> 'k
         ; build_Big_int : 'd -> 'h -> 'l
         ; build_Block : 'd -> 'g list -> 'g
         ; build_Card : 'd -> 'm -> 'h
         ; build_Compr : 'd -> (bool * int * 'm) list -> 'g list -> 'l
         ; build_Diff : 'd -> 'n
         ; build_Div : 'd -> 'i
         ; build_F : 'd -> 'o
         ; build_FIte : 'd -> 'g -> 'g -> 'g -> 'g
         ; build_False : 'd -> 'g
         ; build_G : 'd -> 'o
         ; build_Gt : 'd -> 'p
         ; build_Gte : 'd -> 'p
         ; build_H : 'd -> 'o
         ; build_IBin : 'd -> 'h -> 'i -> 'h -> 'h
         ; build_IComp : 'd -> 'h -> 'p -> 'h -> 'g
         ; build_IEq : 'd -> 'p
         ; build_INEq : 'd -> 'p
         ; build_IUn : 'd -> 'q -> 'h -> 'h
         ; build_Iden : 'd -> 'l
         ; build_Iff : 'd -> 'k
         ; build_Imp : 'd -> 'k
         ; build_In : 'd -> 'r
         ; build_Inter : 'd -> 'n
         ; build_Join : 'd -> 'n
         ; build_LBin : 'd -> 'g -> 'k -> 'g -> 'g
         ; build_LProj : 'd -> 'n
         ; build_LUn : 'd -> 'o -> 'g -> 'g
         ; build_Lshift : 'd -> 'i
         ; build_Lt : 'd -> 'p
         ; build_Lte : 'd -> 'p
         ; build_Mul : 'd -> 'i
         ; build_Name : 'd -> Name.t -> 'l
         ; build_Neg : 'd -> 'q
         ; build_No : 'd -> 'j
         ; build_None_ : 'd -> 'l
         ; build_Not : 'd -> 'o
         ; build_NotIn : 'd -> 'r
         ; build_Num : 'd -> int -> 'h
         ; build_O : 'd -> 'o
         ; build_Or : 'd -> 'k
         ; build_Over : 'd -> 'n
         ; build_P : 'd -> 'o
         ; build_Prime : 'd -> 'm -> 'l
         ; build_Prod : 'd -> 'n
         ; build_Quant : 'd -> 'j -> bool * int * 'm -> 'g list -> 'g
         ; build_R : 'd -> 'k
         ; build_RBin : 'd -> 'm -> 'n -> 'm -> 'l
         ; build_RComp : 'd -> 'm -> 'r -> 'm -> 'g
         ; build_REq : 'd -> 'r
         ; build_RIte : 'd -> 'g -> 'm -> 'm -> 'l
         ; build_RNEq : 'd -> 'r
         ; build_RProj : 'd -> 'n
         ; build_RTClos : 'd -> 's
         ; build_RUn : 'd -> 's -> 'm -> 'l
         ; build_Rem : 'd -> 'i
         ; build_S : 'd -> 'k
         ; build_Sershift : 'd -> 'i
         ; build_Small_int : 'd -> 'm -> 'h
         ; build_Some_ : 'd -> 'j
         ; build_Sub : 'd -> 'i
         ; build_Sum : 'd -> 'm -> 'h -> 'h
         ; build_T : 'd -> 'k
         ; build_TClos : 'd -> 's
         ; build_Transpose : 'd -> 's
         ; build_True : 'd -> 'g
         ; build_U : 'd -> 'k
         ; build_Union : 'd -> 'n
         ; build_Univ : 'd -> 'l
         ; build_Var : 'd -> int -> 'l
         ; build_X : 'd -> 'o
         ; build_Zershift : 'd -> 'i
         ; build_oexp : 'd -> 'l -> int -> 'm
         ; visit_'exp : 'd -> exp -> 'm
         ; visit_'fml : 'd -> fml -> 'g
         ; visit_'iexp : 'd -> iexp -> 'h
         ; visit_AIte : 'd -> fml -> iexp -> iexp -> 'h
         ; visit_Add : 'd -> 'i
         ; visit_All : 'd -> 'j
         ; visit_And : 'd -> 'k
         ; visit_Big_int : 'd -> iexp -> 'l
         ; visit_Block : 'd -> fml list -> 'g
         ; visit_Card : 'd -> exp -> 'h
         ; visit_Compr : 'd -> (bool * int * exp) list -> fml list -> 'l
         ; visit_Diff : 'd -> 'n
         ; visit_Div : 'd -> 'i
         ; visit_F : 'd -> 'o
         ; visit_FIte : 'd -> fml -> fml -> fml -> 'g
         ; visit_False : 'd -> 'g
         ; visit_G : 'd -> 'o
         ; visit_Gt : 'd -> 'p
         ; visit_Gte : 'd -> 'p
         ; visit_H : 'd -> 'o
         ; visit_IBin : 'd -> iexp -> ibinop -> iexp -> 'h
         ; visit_IComp : 'd -> iexp -> icomp_op -> iexp -> 'g
         ; visit_IEq : 'd -> 'p
         ; visit_INEq : 'd -> 'p
         ; visit_IUn : 'd -> iunop -> iexp -> 'h
         ; visit_Iden : 'd -> 'l
         ; visit_Iff : 'd -> 'k
         ; visit_Imp : 'd -> 'k
         ; visit_In : 'd -> 'r
         ; visit_Inter : 'd -> 'n
         ; visit_Join : 'd -> 'n
         ; visit_LBin : 'd -> fml -> lbinop -> fml -> 'g
         ; visit_LProj : 'd -> 'n
         ; visit_LUn : 'd -> lunop -> fml -> 'g
         ; visit_Lshift : 'd -> 'i
         ; visit_Lt : 'd -> 'p
         ; visit_Lte : 'd -> 'p
         ; visit_Mul : 'd -> 'i
         ; visit_Name : 'd -> Name.t -> 'l
         ; visit_Neg : 'd -> 'q
         ; visit_No : 'd -> 'j
         ; visit_None_ : 'd -> 'l
         ; visit_Not : 'd -> 'o
         ; visit_NotIn : 'd -> 'r
         ; visit_Num : 'd -> int -> 'h
         ; visit_O : 'd -> 'o
         ; visit_Or : 'd -> 'k
         ; visit_Over : 'd -> 'n
         ; visit_P : 'd -> 'o
         ; visit_Prime : 'd -> exp -> 'l
         ; visit_Prod : 'd -> 'n
         ; visit_Quant : 'd -> quant -> bool * int * exp -> fml list -> 'g
         ; visit_R : 'd -> 'k
         ; visit_RBin : 'd -> exp -> rbinop -> exp -> 'l
         ; visit_RComp : 'd -> exp -> comp_op -> exp -> 'g
         ; visit_REq : 'd -> 'r
         ; visit_RIte : 'd -> fml -> exp -> exp -> 'l
         ; visit_RNEq : 'd -> 'r
         ; visit_RProj : 'd -> 'n
         ; visit_RTClos : 'd -> 's
         ; visit_RUn : 'd -> runop -> exp -> 'l
         ; visit_Rem : 'd -> 'i
         ; visit_S : 'd -> 'k
         ; visit_Sershift : 'd -> 'i
         ; visit_Small_int : 'd -> exp -> 'h
         ; visit_Some_ : 'd -> 'j
         ; visit_Sub : 'd -> 'i
         ; visit_Sum : 'd -> exp -> iexp -> 'h
         ; visit_T : 'd -> 'k
         ; visit_TClos : 'd -> 's
         ; visit_Transpose : 'd -> 's
         ; visit_True : 'd -> 'g
         ; visit_U : 'd -> 'k
         ; visit_Union : 'd -> 'n
         ; visit_Univ : 'd -> 'l
         ; visit_Var : 'd -> int -> 'l
         ; visit_X : 'd -> 'o
         ; visit_Zershift : 'd -> 'i
         ; visit_comp_op : 'd -> comp_op -> 'r
         ; visit_exp : 'd -> exp -> 'm
         ; visit_fml : 'd -> fml -> 'g
         ; visit_ibinop : 'd -> ibinop -> 'i
         ; visit_icomp_op : 'd -> icomp_op -> 'p
         ; visit_iexp : 'd -> iexp -> 'h
         ; visit_iunop : 'd -> iunop -> 'q
         ; visit_lbinop : 'd -> lbinop -> 'k
         ; visit_lunop : 'd -> lunop -> 'o
         ; visit_oexp : 'd -> (fml, exp, iexp) oexp -> 'm
         ; visit_ofml : 'd -> (fml, exp, iexp) ofml -> 'g
         ; visit_oiexp : 'd -> (fml, exp, iexp) oiexp -> 'h
         ; visit_prim_oexp : 'd -> (fml, exp, iexp) prim_oexp -> 'l
         ; visit_quant : 'd -> quant -> 'j
         ; visit_rbinop : 'd -> rbinop -> 'n
         ; visit_runop : 'd -> runop -> 's
         ; .. >

    method virtual build_AIte : 'd -> 'g -> 'h -> 'h -> 'h
    method virtual build_Add : 'd -> 'i
    method virtual build_All : 'd -> 'j
    method virtual build_And : 'd -> 'k
    method virtual build_Big_int : 'd -> 'h -> 'l
    method virtual build_Block : 'd -> 'g list -> 'g
    method virtual build_Card : 'd -> 'm -> 'h
    method virtual build_Compr : 'd -> (bool * int * 'm) list -> 'g list -> 'l
    method virtual build_Diff : 'd -> 'n
    method virtual build_Div : 'd -> 'i
    method virtual build_F : 'd -> 'o
    method virtual build_FIte : 'd -> 'g -> 'g -> 'g -> 'g
    method virtual build_False : 'd -> 'g
    method virtual build_G : 'd -> 'o
    method virtual build_Gt : 'd -> 'p
    method virtual build_Gte : 'd -> 'p
    method virtual build_H : 'd -> 'o
    method virtual build_IBin : 'd -> 'h -> 'i -> 'h -> 'h
    method virtual build_IComp : 'd -> 'h -> 'p -> 'h -> 'g
    method virtual build_IEq : 'd -> 'p
    method virtual build_INEq : 'd -> 'p
    method virtual build_IUn : 'd -> 'q -> 'h -> 'h
    method virtual build_Iden : 'd -> 'l
    method virtual build_Iff : 'd -> 'k
    method virtual build_Imp : 'd -> 'k
    method virtual build_In : 'd -> 'r
    method virtual build_Inter : 'd -> 'n
    method virtual build_Join : 'd -> 'n
    method virtual build_LBin : 'd -> 'g -> 'k -> 'g -> 'g
    method virtual build_LProj : 'd -> 'n
    method virtual build_LUn : 'd -> 'o -> 'g -> 'g
    method virtual build_Lshift : 'd -> 'i
    method virtual build_Lt : 'd -> 'p
    method virtual build_Lte : 'd -> 'p
    method virtual build_Mul : 'd -> 'i
    method virtual build_Name : 'd -> Name.t -> 'l
    method virtual build_Neg : 'd -> 'q
    method virtual build_No : 'd -> 'j
    method virtual build_None_ : 'd -> 'l
    method virtual build_Not : 'd -> 'o
    method virtual build_NotIn : 'd -> 'r
    method virtual build_Num : 'd -> int -> 'h
    method virtual build_O : 'd -> 'o
    method virtual build_Or : 'd -> 'k
    method virtual build_Over : 'd -> 'n
    method virtual build_P : 'd -> 'o
    method virtual build_Prime : 'd -> 'm -> 'l
    method virtual build_Prod : 'd -> 'n
    method virtual build_Quant : 'd -> 'j -> bool * int * 'm -> 'g list -> 'g
    method virtual build_R : 'd -> 'k
    method virtual build_RBin : 'd -> 'm -> 'n -> 'm -> 'l
    method virtual build_RComp : 'd -> 'm -> 'r -> 'm -> 'g
    method virtual build_REq : 'd -> 'r
    method virtual build_RIte : 'd -> 'g -> 'm -> 'm -> 'l
    method virtual build_RNEq : 'd -> 'r
    method virtual build_RProj : 'd -> 'n
    method virtual build_RTClos : 'd -> 's
    method virtual build_RUn : 'd -> 's -> 'm -> 'l
    method virtual build_Rem : 'd -> 'i
    method virtual build_S : 'd -> 'k
    method virtual build_Sershift : 'd -> 'i
    method virtual build_Small_int : 'd -> 'm -> 'h
    method virtual build_Some_ : 'd -> 'j
    method virtual build_Sub : 'd -> 'i
    method virtual build_Sum : 'd -> 'm -> 'h -> 'h
    method virtual build_T : 'd -> 'k
    method virtual build_TClos : 'd -> 's
    method virtual build_Transpose : 'd -> 's
    method virtual build_True : 'd -> 'g
    method virtual build_U : 'd -> 'k
    method virtual build_Union : 'd -> 'n
    method virtual build_Univ : 'd -> 'l
    method virtual build_Var : 'd -> int -> 'l
    method virtual build_X : 'd -> 'o
    method virtual build_Zershift : 'd -> 'i
    method virtual build_oexp : 'd -> 'l -> int -> 'm
    method visit_'exp : 'd -> exp -> 'm
    method visit_'fml : 'd -> fml -> 'g
    method visit_'iexp : 'd -> iexp -> 'h
    method visit_AIte : 'd -> fml -> iexp -> iexp -> 'h
    method visit_Add : 'd -> 'i
    method visit_All : 'd -> 'j
    method visit_And : 'd -> 'k
    method visit_Big_int : 'd -> iexp -> 'l
    method visit_Block : 'd -> fml list -> 'g
    method visit_Card : 'd -> exp -> 'h
    method visit_Compr : 'd -> (bool * int * exp) list -> fml list -> 'l
    method visit_Diff : 'd -> 'n
    method visit_Div : 'd -> 'i
    method visit_F : 'd -> 'o
    method visit_FIte : 'd -> fml -> fml -> fml -> 'g
    method visit_False : 'd -> 'g
    method visit_G : 'd -> 'o
    method visit_Gt : 'd -> 'p
    method visit_Gte : 'd -> 'p
    method visit_H : 'd -> 'o
    method visit_IBin : 'd -> iexp -> ibinop -> iexp -> 'h
    method visit_IComp : 'd -> iexp -> icomp_op -> iexp -> 'g
    method visit_IEq : 'd -> 'p
    method visit_INEq : 'd -> 'p
    method visit_IUn : 'd -> iunop -> iexp -> 'h
    method visit_Iden : 'd -> 'l
    method visit_Iff : 'd -> 'k
    method visit_Imp : 'd -> 'k
    method visit_In : 'd -> 'r
    method visit_Inter : 'd -> 'n
    method visit_Join : 'd -> 'n
    method visit_LBin : 'd -> fml -> lbinop -> fml -> 'g
    method visit_LProj : 'd -> 'n
    method visit_LUn : 'd -> lunop -> fml -> 'g
    method visit_Lshift : 'd -> 'i
    method visit_Lt : 'd -> 'p
    method visit_Lte : 'd -> 'p
    method visit_Mul : 'd -> 'i
    method visit_Name : 'd -> Name.t -> 'l
    method visit_Neg : 'd -> 'q
    method visit_No : 'd -> 'j
    method visit_None_ : 'd -> 'l
    method visit_Not : 'd -> 'o
    method visit_NotIn : 'd -> 'r
    method visit_Num : 'd -> int -> 'h
    method visit_O : 'd -> 'o
    method visit_Or : 'd -> 'k
    method visit_Over : 'd -> 'n
    method visit_P : 'd -> 'o
    method visit_Prime : 'd -> exp -> 'l
    method visit_Prod : 'd -> 'n
    method visit_Quant : 'd -> quant -> bool * int * exp -> fml list -> 'g
    method visit_R : 'd -> 'k
    method visit_RBin : 'd -> exp -> rbinop -> exp -> 'l
    method visit_RComp : 'd -> exp -> comp_op -> exp -> 'g
    method visit_REq : 'd -> 'r
    method visit_RIte : 'd -> fml -> exp -> exp -> 'l
    method visit_RNEq : 'd -> 'r
    method visit_RProj : 'd -> 'n
    method visit_RTClos : 'd -> 's
    method visit_RUn : 'd -> runop -> exp -> 'l
    method visit_Rem : 'd -> 'i
    method visit_S : 'd -> 'k
    method visit_Sershift : 'd -> 'i
    method visit_Small_int : 'd -> exp -> 'h
    method visit_Some_ : 'd -> 'j
    method visit_Sub : 'd -> 'i
    method visit_Sum : 'd -> exp -> iexp -> 'h
    method visit_T : 'd -> 'k
    method visit_TClos : 'd -> 's
    method visit_Transpose : 'd -> 's
    method visit_True : 'd -> 'g
    method visit_U : 'd -> 'k
    method visit_Union : 'd -> 'n
    method visit_Univ : 'd -> 'l
    method visit_Var : 'd -> int -> 'l
    method visit_X : 'd -> 'o
    method visit_Zershift : 'd -> 'i

    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_comp_op : 'd -> comp_op -> 'r
    method visit_exp : 'd -> exp -> 'm
    method private visit_float : 'env. 'env -> float -> float
    method visit_fml : 'd -> fml -> 'g
    method visit_ibinop : 'd -> ibinop -> 'i
    method visit_icomp_op : 'd -> icomp_op -> 'p
    method visit_iexp : 'd -> iexp -> 'h
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_iunop : 'd -> iunop -> 'q

    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t

    method visit_lbinop : 'd -> lbinop -> 'k

    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method visit_lunop : 'd -> lunop -> 'o
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_oexp : 'd -> (fml, exp, iexp) oexp -> 'm
    method visit_ofml : 'd -> (fml, exp, iexp) ofml -> 'g
    method visit_oiexp : 'd -> (fml, exp, iexp) oiexp -> 'h

    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    method visit_prim_oexp : 'd -> (fml, exp, iexp) prim_oexp -> 'l
    method visit_quant : 'd -> quant -> 'j
    method visit_rbinop : 'd -> rbinop -> 'n

    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

    method private visit_result :
      'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b) ->
      ('env -> 'e -> 'f) ->
      'env ->
      ('a, 'e) result ->
      ('b, 'f) result

    method visit_runop : 'd -> runop -> 's
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
