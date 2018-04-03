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

(** Definition of the type for Electrod models.  *)

open Containers


(* type of (well-formed) Electrod models *)
type t = {
  file : string option;
  (* table of relations indexed by names (remark: a {!Relation.t} also knows its
     own name) *)
  domain : Domain.t;
  instance : Instance.t;
  sym : Symmetry.t list;
  invariants : Elo_goal.fml list; 
  goal : Elo_goal.t;
  atom_renaming : (Atom.t, Atom.t) List.Assoc.t;
  name_renaming : (Name.t, Name.t) List.Assoc.t;
}

let make file domain instance sym invariants goal =
  { file; domain; instance; sym;
    invariants; goal; atom_renaming = [];
    name_renaming = [] }


let pp out { domain; instance; invariants; goal; _ } =
  let open Fmtc in
  pf out "%a@\n%a@\n%a@\n%a"
    Domain.pp domain
    Instance.pp instance
    (vbox2
     @@ styled `Bold
     @@ const string "invariant"
        **< cut
        **< (Fmtc.list @@ Elo_goal.pp_fml 0)) invariants
    (vbox @@ Elo_goal.pp) goal
