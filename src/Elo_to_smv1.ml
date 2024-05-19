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

(** Provides a transformation from Electrod models to SMV models. Uses
   enumerations when possible. *)

open Containers

module SMV_atom : Solver.ATOMIC_PROPOSITION = struct
  type t = {
    sym : Symbol.t;
    (* hashconsed strings *)
    const : bool;
    partial : bool;
    (* is 'lone'? *)
    dom_arity : int option;
        (* arity of the domain (>=0) if functional, else None *)
  }

  let compare { sym = sym1; _ } { sym = sym2; _ } = Symbol.compare sym1 sym2

  let compare_string { sym = sym1; _ } { sym = sym2; _ } =
    Symbol.compare_string sym1 sym2

  let pp fmt at = Symbol.pp fmt at.sym
  let equal { sym = sym1; _ } { sym = sym2; _ } = Symbol.equal sym1 sym2
  let hash at = Symbol.hash at.sym
  let domain_arity t = t.dom_arity
  let is_const t = t.const
  let is_partial t = t.partial

  (* table tracking which pair (name, tuple) a string comes from. Uses
     hahsconsing to make this more efficient *)
  module HT = CCHashtbl.Make (Symbol)

  let names_and_tuples = HT.create 179

  (* usually less than that many VARs *)

  let rel_sep = "-"
  let atom_sep = Fmtc.minus

  let make domain =
    let name_tuple (name, tuple) =
      let rel = Domain.get_exn name domain in
      let dom_arity =
        let open Scope in
        match Relation.scope rel with
        | Exact _ -> assert false
        | Inexact (Plain_relation _) -> None
        | Inexact (Partial_function (ar, _)) -> Some ar
        | Inexact (Total_function (ar, _)) -> Some ar
      in
      let const = Relation.is_const rel in
      let partial = rel |> Relation.scope |> Scope.is_partial in
      let ats = Tuple.to_list tuple in
      let name_str =
        let s = Fmtc.to_to_string Name.pp name in
        if String.prefix ~pre:"$" s then
          (* Skolem vars may have a name incompatible with SMV so: *)
          "_" ^ s
        else s
      in
      let full_str =
        Format.sprintf "%s%s%a" name_str rel_sep
          Fmtc.(list ~sep:atom_sep Atom.pp)
          ats
      in
      let sym = Symbol.make full_str in
      (* keep track of creations to allow to get original pairs back *)
      (* Note: this is an effect but it's fine with the cache hereunder as we want the same symbol to be used for the same name and tuple. *)
      HT.add names_and_tuples sym (name, tuple);
      { sym; dom_arity; const; partial }
    in
    let cache =
      CCCache.unbounded
        ~eq:(Pair.equal Name.equal Tuple.equal)
        ~hash:(Hash.pair Name.hash Tuple.hash)
        1793
    in
    fun name tuple -> CCCache.with_cache cache name_tuple (name, tuple)

  let split at = HT.get names_and_tuples at.sym
  let split_string str = HT.get names_and_tuples (Symbol.make str)
end

module SMV_LTL = Smv.Make_SMV_LTL (SMV_atom)
module SMV_file_format = Smv.Make_SMV_file_format (SMV_LTL)
module Elo_to_SMV_LTL = Elo_to_ltl1.Make (SMV_LTL)

module Elo_to_SMV_model =
  Elo_to_model1.Make (SMV_LTL) (Elo_to_SMV_LTL) (SMV_file_format)

let pp = SMV_file_format.pp
let analyze = SMV_file_format.analyze

(* temporary *)
let run (elo, temporal_symmetry, symmetry_offset) =
  Elo_to_SMV_model.run (elo, temporal_symmetry, symmetry_offset)

let transfo = Transfo.make "to_smv1" run

let%expect_test _ =
  let open SMV_LTL in
  let test_formula =
    let a = auxiliary @@ Symbol.make "a" in
    let c = auxiliary @@ Symbol.make "c" in
    let f1 = ifthenelse_arith (next c) (num 4 0) (num 4 1) in
    let f2 = eventually @@ next @@ comp eq f1 (num 4 1) in
    let f3 = ifthenelse_arith f2 (num 4 2) (num 4 3) in
    let g = ifthenelse_arith (eventually a) (num 4 4) (num 4 5) in
    comp eq f3 g
  in
  pp Fmt.stdout 2 test_formula;
  [%expect
    {|
    (((F (X (((X c)) ? (0sd2_0) : (0sd2_1)) = 0sd2_1))) ? (0sd2_2) : (0sd2_3)) =
      (((F a)) ? (0sd2_4) : (0sd2_5)) |}];
  Fmt.(pf stdout) "LTLSPEC@\n";
  stratify ~smv_section:`Ltlspec (always @@ test_formula)
  |> List.iter (fun f ->
         pp Fmt.stdout 2 f;
         Fmtc.(hardline stdout ()));
  [%expect
    {|
    LTLSPEC
    (G (__aux0 <-> (F a)))
    (G (__aux2 <-> (X c)))
    (G (__aux1 <-> (F (X ((__aux2) ? (0sd2_0) : (0sd2_1)) = 0sd2_1))))
    (G ((__aux1) ? (0sd2_2) : (0sd2_3)) = ((__aux0) ? (0sd2_4) : (0sd2_5))) |}];
  Fmt.(pf stdout) "TRANS@\n";
  stratify ~smv_section:`Trans test_formula
  |> List.iter (fun f ->
         pp Fmt.stdout 2 f;
         Fmtc.(hardline stdout ()));
  [%expect
    {|
      TRANS
      (__aux3 <-> (F a))
      (__aux5 <-> (X c))
      (__aux4 <-> (F (X ((__aux5) ? (0sd2_0) : (0sd2_1)) = 0sd2_1)))
      ((__aux4) ? (0sd2_2) : (0sd2_3)) = ((__aux3) ? (0sd2_4) : (0sd2_5)) |}]
