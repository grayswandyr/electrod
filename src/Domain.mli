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

(** The domain represents the set of relation declarations. It also contains the bitwidth representing integers. *)

open Containers

type t
(** Virtually: a map between relation names and their definition as sets of
    tuples. *)

val empty : t
(** Constructor. *)

val add : Name.t -> Relation.t -> t -> t
(** Adds an association to the domain. 
    The name must not be in the domain already. *)

val remove : Name.t -> t -> t
(** Removes a name from the domain, if it is present.  *)

val mem : Name.t -> t -> bool
(** Checks whether a name is already bound in the map. *)

val compute_bitwidth : Tuple_set.t -> t -> t
(** The tuple set is the value of `univ`. 
    
Pre: the Name for the integer set must be present in the domain. Raises otherwise. Also raises if the size of the integer set isn't a power of 2 or doesn't contain all integers between -2^bitwidth and +2^bitwidth-1. *)

val bitwidth : t -> int
(** Returns the bitwidth (a non-negative power of 2).  *)

(** {1 Accessors}*)

val get_exn : Name.t -> t -> Relation.t
(** May raise Not_found.  *)

val get : Name.t -> t -> Relation.t option
(** May rather return None. *)

val univ_atoms : t -> Tuple_set.t
(** Returns the set of atoms in univ, represented as a {!type:Tuple_set.t} *)

val to_list : t -> (Name.t * Relation.t) list
(** Returns the map as an association list *)

val of_list : int -> (Name.t * Relation.t) list -> t
val equal : t -> t -> bool

val must : Name.t -> t -> Tuple_set.t
(** Returns the "may" and "must" tuple sets associated to a relation name.  *)

val may : Name.t -> t -> Tuple_set.t
val sup : Name.t -> t -> Tuple_set.t

val musts : ?with_univ_and_ident:bool -> t -> (Name.t, Tuple_set.t) List.Assoc.t
(** Returns the association list between relation names and their "must" set.  *)

val arities : t -> (Name.t, int) List.Assoc.t
(** Returns the association list between relation names and their arity.  *)

val rename :
  (Atom.t, Atom.t) List.Assoc.t -> (Name.t, Name.t) List.Assoc.t -> t -> t
(** Returns a new domain where atoms and relations have been renamed.  *)

val update_domain_with_instance : t -> Instance.t -> t
(** For every entry in [inst], [update_domain_with_instance dom inst] replaces
    the corresponding relation in [dom] with the exact scope given by [inst]. *)

val ints : t -> Tuple_set.t
(** Returns the set of ints as 1-tuples  *)

val shl : t -> Tuple_set.t option
(** Returns this shift as a set of triples (None if it doesn't exist). *)

val shr : t -> Tuple_set.t option
(** Returns this shift as a set of triples (None if it doesn't exist). *)

val sha : t -> Tuple_set.t option
(** Returns this shift as a set of triples (None if it doesn't exist). *)

include Intf.Print.S with type t := t
