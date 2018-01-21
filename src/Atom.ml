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
 * License-Filename: LICENSES/MPL-2.0.txt
 ******************************************************************************)

open Containers

type t = {
  sym : Sym.t;
  loc : Loc.t option
}

let compare a1 a2 = Sym.compare a1.sym a2.sym

let equal a1 a2 =
  Sym.equal a1.sym a2.sym
  (* |> Fun.tap *)
  (*      (fun res -> *)
  (*         (Fmt.epr "Atom.equal %a %a = %B" *)
  (*            Sym.pp a1.sym *)
  (*            Sym.pp a2.sym *)
  (*            res)) *)

let atom ?loc s = { sym = Sym.make s; loc }

let of_raw_ident id =
  atom ~loc:(Raw_ident.location id) (Raw_ident.basename id)
 

let hash atom = Sym.hash atom.sym

(** Generic interface implementations *)

let pp out { sym; _ } =
  Sym.pp out sym



module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
 



let pp_list atoms =
  Fmtc.(braces_ @@ list ~sep:sp pp) atoms

let to_string_list = Fmtc.to_to_string pp_list
