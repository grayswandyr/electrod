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

(** Relation scopes. *)

type t = private
  | Exact of TupleSet.t             (** means: lower bound = upper bound *)
  | Inexact of TupleSet.t * TupleSet.t (** invariant: lower bound <> upper bound *)


(** {1 Constructors} *)
                              
val exact : TupleSet.t -> t
val inexact : TupleSet.t -> TupleSet.t -> t

(** [included_in ts scope] tells whether [ts] is in the scope (meaning
    it also contains the lower bound of the scope if the latter is
    inexact.) *)
val included_in : TupleSet.t -> t -> bool

(** Return the inf and sup bounds of the scope. *)
val inf : t -> TupleSet.t
val sup : t -> TupleSet.t

(** Return the must and may (= sup - inf; computation is {b cached})
    bounds of the scope. *)
val must : t -> TupleSet.t
val may : t -> TupleSet.t


(** 0 if the arity cannot be inferred (= is unknown), [n > 0] otherwise. *)
val inferred_arity : t -> int 



include Intf.Print.S with type t := t


