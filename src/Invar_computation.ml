(*******************************************************************************
 * Time-stamp: <2017-11-14 CET 14:06:50 David Chemouil>
 * 
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2017 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSES/MPL-2.0.txt
 ******************************************************************************)

open Containers

(* 
Static_prop: a proposition that does not include any variable relation
nor any temporal operator.
Primed_prop: a propostion that may include variabale and constant
relations, without any temporal operator except un-nested X (next) or
prime.
Invar: proposition of the form always (phi) where phi does not include
any temporal operator (the color pf phi is Init or Static_prop).
Init: proposition without any temporal operator.
Trans: proposition of the form always (phi) where the color of phi is
Primed_prop.
Temporal: any other proposition.
 *)
type goal_color = Static_prop | Primed_prop | Invar | Init | Trans | Temporal

let to_string (gc : goal_color) =
  match gc with
  | Static_prop -> "Static_prop"
  | Primed_prop -> "Primed_prop"
  | Invar -> "Invar"
  | Init -> "Init"
  | Trans -> "Trans"
  | Temporal -> "Temporal"
let pp out gc =
  Fmtc.(pf out "%a" (styled `Yellow string) (to_string gc))

(* Computes the most general color between two colors *)
let max_color c1 c2 =
  match c1, c2 with 
  | Static_prop, Static_prop -> Static_prop
  | (Init | Static_prop) , (Init | Static_prop)  -> Init
  | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop)
    -> Primed_prop                                
  | (Invar | Static_prop) , (Invar | Static_prop) -> Invar
  | (Trans | Static_prop) , (Trans | Static_prop) -> Trans
  | _ , _ -> Temporal

(* same as max_color, but without Invar nor Trans *)               
let max_color_wiwt c1 c2 =
  match c1, c2 with
  | Static_prop, Static_prop -> Static_prop
  | (Init | Static_prop) , (Init | Static_prop)  -> Init
  | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop)
    -> Primed_prop 
  | _ , _ -> Temporal

(* removes the top level always operator in an invariant elo formula *)
let rec remove_always_to_invar f =
  let open GenGoal in
  let {prim_fml; fml_loc} = f in
  match prim_fml with
    | LUn (G, subfml) -> subfml
    | LBin (fml1, And, fml2) ->
        let fml1' = remove_always_to_invar fml1 in
        let fml2' = remove_always_to_invar fml2 in
        {prim_fml= lbinary fml1' And fml2'; fml_loc}
    | _ -> f

(* adds an always operator to an (invariant) elo formula if the
   outermost operator is not an always *)
let rec add_always_to_invar f =
  let open GenGoal in
  let {prim_fml; fml_loc} = f in
  match prim_fml with
    | LUn (G, _) -> f
    | _ -> {prim_fml = lunary G f; fml_loc}


class ['env] invarComputation = object (self : 'self)
  inherit ['self] GenGoal.fold as super

  method visit_'v (env : 'env) = Fun.id

  method visit_'i (env : 'env) = Fun.id

  method build_fml (env : 'env) f' _ = f' 

  method build_Run (env : 'env) blk' =
    List.fold_left
      max_color_wiwt
      Static_prop
      blk'

  method build_Check (env : 'env) blk' =
    List.fold_left
      max_color_wiwt
      Static_prop
      blk'


  method build_True (env : 'env) = Static_prop

  method build_False (env : 'env) = Static_prop

  method build_Block (env : 'env) blk_colors =
    List.fold_left
      max_color
      Static_prop
      blk_colors

  method build_FIte (env : 'env) f t e =
    match f, t, e with
    | Static_prop, Static_prop, Static_prop -> Static_prop
    | (Init | Static_prop) , (Init | Static_prop) , (Init | Static_prop)
      -> Init
    | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop) ,
      (Primed_prop | Init | Static_prop)
      -> Primed_prop
    | _ , _, _ -> Temporal

  method build_Let (env : 'env) bs' block'= assert false (* SIMPLIFIED *)

  (* quant *)

  method build_Quant (env : 'env) quant' sim_bindings_colors blk_colors =
    let blk_color =
      List.fold_left
        max_color_wiwt
        Static_prop
        blk_colors
    in
    let max_color_for_simbindings color_acc (disj1, vars1, e1)  =
      max_color_wiwt color_acc e1
    in      
    let sim_bindings_color =
      List.fold_left
        max_color_for_simbindings
        Static_prop
        sim_bindings_colors
    in
    quant' sim_bindings_color blk_color

  method build_One (env : 'env) = max_color

  method build_Lone (env : 'env) = max_color

  method build_All (env : 'env) =  max_color

  method build_No (env : 'env) =  max_color

  method build_Some_ (env : 'env) =  max_color    

  (* lbinop *)      

  method build_LBin (env : 'env) f1' op' f2' = op' f1' f2'

  method build_And (env : 'env) = max_color

  method build_Iff (env : 'env) = max_color_wiwt

  method build_Imp (env : 'env)  = max_color_wiwt

  method build_U (env : 'env) _ _ = Temporal

  method build_Or (env : 'env)  = max_color_wiwt

  method build_R (env : 'env) _ _ = Temporal

  method build_S (env : 'env) _ _ = Temporal

  (* lunop *)                     

  method build_LUn (env : 'env) op' f' =
    op' f'

  method build_X (env : 'env) f' =
    match f' with
    | Static_prop | Init -> Primed_prop
    | _ -> Temporal

  method build_F (env : 'env) _ = Temporal

  method build_G (env : 'env) f' =
    match f' with
    | Init | Static_prop | Invar -> Invar
    | Primed_prop | Trans -> Trans 
    | _ -> Temporal

  method build_H (env : 'env) _ = Temporal

  method build_O (env : 'env) _ = Temporal

  method build_P (env : 'env) _ = Temporal

  method build_Not (env : 'env) f' =
    max_color_wiwt Static_prop f'

  (* compo_op *)

  method build_RComp (env : 'env) f1' op' f2' =
    op' f1' f2'

  method build_REq (env : 'env) = max_color_wiwt

  method build_In (env : 'env)  = max_color_wiwt

  method build_NotIn (env : 'env) = max_color_wiwt

  method build_RNEq (env : 'env) = max_color_wiwt

  (* icomp_op *)

  method build_IComp (env : 'env) e1' op' e2' =
    op' e1' e2'

  method build_Gt (env : 'env) = max_color_wiwt

  method build_Gte (env : 'env) = max_color_wiwt

  method build_IEq (env : 'env) = max_color_wiwt

  method build_INEq (env : 'env) = max_color_wiwt

  method build_Lt (env : 'env) = max_color_wiwt

  method build_Lte (env : 'env) = max_color_wiwt

  (* rqualify *)

  method build_Qual (env : 'env) (q' : bool) (r' : goal_color) = assert false (* SIMPLIFIED *)

  method build_RLone (env : 'env) = assert false (* SIMPLIFIED *)

  method build_RNo (env : 'env) = assert false (* SIMPLIFIED *)

  method build_ROne (env : 'env) = assert false (* SIMPLIFIED *)

  method build_RSome (env : 'env) = assert false (* SIMPLIFIED *)

  (************************** exp  ********************************)

  method build_exp (env : 'env) pe' _ _ = pe'

  method build_Compr (env : 'env) sbs' b' =
    let blk_color =
      List.fold_left
        max_color_wiwt
        Static_prop
        b'
    in

    let max_color_for_simbindings (color_acc : goal_color)
          ((disj1, vars1, e1) : bool * Elo.var list * goal_color)  =
      max_color_wiwt color_acc e1
    in      
    let sim_bindings_color =
      List.fold_left
        max_color_for_simbindings
        Static_prop
        sbs'
    in

    max_color_wiwt sim_bindings_color blk_color

  method build_Iden (env : 'env) = Static_prop

  method build_BoxJoin (env : 'env) call' args' = (* SIMPLIFIED *)
    assert false

  method build_Ident (env : 'env) id =
    let open Elo in
    match id with
      | Var v ->
          Static_prop
      | Name r ->
          if env#is_const r then
            Static_prop
          else
            Init

  method build_None_ (env : 'env) = Static_prop

  method build_Univ (env : 'env) = Static_prop

  method build_Prime (env : 'env) f' =
    match f' with
    | Static_prop | Init -> Primed_prop
    | _ -> Temporal

  method build_RIte (env : 'env) f' t' e' =
    match f', t', e' with
    | Static_prop, Static_prop, Static_prop -> Static_prop
    | (Init | Static_prop) , (Init | Static_prop) , (Init | Static_prop)
      -> Init
    | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop) ,
      (Primed_prop | Init | Static_prop)
      -> Primed_prop
    | _ , _, _ -> Temporal


  (* rbinop *)

  method build_RBin (env : 'env) f1' op' f2' =
    op' f1' f2'

  method build_Union (env : 'env)  = max_color_wiwt


  method build_Inter (env : 'env) = max_color_wiwt

  method build_Join (env : 'env) = max_color_wiwt

  method build_LProj (env : 'env) = max_color_wiwt
  method build_Prod (env : 'env) = max_color_wiwt

  method build_RProj (env : 'env) = max_color_wiwt
  method build_Diff (env : 'env) = max_color_wiwt
  method build_Over (env : 'env) = max_color_wiwt                           
  (* runop *)

  method build_RUn (env : 'env) op' e' = op' e'

  method build_RTClos (env : 'env) = Fun.id

  method build_Transpose (env : 'env) = Fun.id

  method build_TClos (env : 'env) = Fun.id


  (*********************************** iexp **************************************)

  method build_iexp (env : 'env) iexp' _ = iexp'

  method build_IBin (env : 'env) i1' op' i2' = op' i1' i2'

  method build_IUn (env : 'env) op' i' = op' i'

  method build_Num (env : 'env) _ = Static_prop

  method build_Add (env : 'env) = max_color_wiwt

  method build_Neg (env : 'env) = max_color_wiwt Static_prop

  method build_Sub (env : 'env) = max_color_wiwt

  method build_Card (env : 'env) r' = r'
end
