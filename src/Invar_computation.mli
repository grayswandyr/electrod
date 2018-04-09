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

(** Helpers for sorting formulas into invariants, and other types of formulas *)

type goal_color = private
    Static_prop
  | Primed_prop
  | Invar
  | Init
  | Trans
  | Temporal
val to_string : goal_color -> string
val pp : Format.formatter -> goal_color -> unit

val remove_always_from_invar : Elo.fml -> Elo.fml
val add_always_to_invar : Elo.fml -> Elo.fml

(** Computes the color of a formula *)
val color : Elo.t -> Elo.fml -> goal_color