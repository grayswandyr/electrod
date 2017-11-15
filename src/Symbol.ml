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

module H = Hashcons

type t = string H.hash_consed

module S = H.Make(struct
    type t = string
    let hash = String.hash
		let equal = String.equal 
  end)
  
(* ********************* *)
(* table for hashconsing *)
(* ********************* *)
let table = S.create 271
(* ********************* *)
           
let make s =
  S.hashcons table s

let hash sym =
  sym.H.hkey

let compare s1 s2 =
  s1.H.tag - s2.H.tag

let equal x1 x2 =
  x1 == x2

let pp out at =
  (* Format.fprintf out "%s/%d" at.H.node at.H.tag *)
  Format.fprintf out "%s" at.H.node 

include Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
