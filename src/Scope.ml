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

module TS = TupleSet
  

type t =
  | Exact of TS.t              
  | Inexact of TS.t * TS.t    

let exact bound = Exact bound

let inexact inf sup =
  assert TS.(inferred_arity inf = inferred_arity sup
             || TS.is_empty inf);
  assert TS.(size sup >= size inf);
  Inexact (inf, sup)

let inferred_arity = function
  | Exact b
  | Inexact (_, b) -> TS.inferred_arity b


let included_in tupleset = function
  | Exact exact ->
      TS.subset tupleset exact
  | Inexact (inf, sup) -> 
      TS.subset inf tupleset
      && TS.subset tupleset sup

let inf = function
  | Exact ts
  | Inexact (ts, _) -> ts
    
let sup = function
  | Exact ts
  | Inexact (_, ts) -> ts

let must = inf


let equal sc1 sc2 = match sc1, sc2 with
  | Exact ts1, Exact ts2 -> TS.equal ts1 ts2
  | Inexact (inf1, sup1), Inexact (inf2, sup2) ->
      TS.(equal inf1 inf2 && equal sup1 sup2)
  | Exact _, Inexact _
  | Inexact _, Exact _ -> false
    
let may_aux sc =
  assert (TS.subset (inf sc) (sup sc));
  match sc with
    | Exact _ ->
        TS.empty 
    | Inexact (inf, sup) ->
        TS.diff sup inf

let may =
  CCCache.(with_cache (lru ~eq:equal 253) may_aux) 


let pp out = function
  | Exact bound -> TS.pp out bound
  | Inexact (inf, sup) ->
      Fmtc.(box @@ pair ~sep:sp (box2 TS.pp) (box2 TS.pp)) out (inf, sup)
        

 

module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
 

