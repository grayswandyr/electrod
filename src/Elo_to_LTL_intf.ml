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

(** Abstract type for a converter from Ast models to (abstract) LTL formulas.  *)

open Containers

module type S = sig
  type atomic                     (* LTL propositional atoms *)
  type ltl                      (* ltl formula *)

  val color :
    Ast.t ->
    (Ast.var, Ast.ident) GenGoal.fml ->
    Invar_computation.goal_color

  val convert :
    Ast.t ->
    (Ast.var, Ast.ident) GenGoal.fml ->
    string * ltl

end
