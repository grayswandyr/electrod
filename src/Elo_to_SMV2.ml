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

(** Provides a transformation from Electrod models to SMV models.  *)

open Containers

  
module SMV_atom : Solver.ATOMIC_PROPOSITION = struct
  type t = Symbol.t             (* hashconsed strings *)
    
  let compare = Symbol.compare

  let pp = Symbol.pp

  let equal = Symbol.equal

  let hash = Symbol.hash


  (* table tracking which pair (name, tuple) a string comes from. Uses
     hahsconsing to make this more efficient *)
  module HT = CCHashtbl.Make(Symbol)
      
  let names_and_tuples = HT.create 179 (* usually less than that many VARs *)
             
  let rel_sep = "-"

  let atom_sep = Fmtc.minus
  
  let make name tuple =
    let ats = Tuple.to_list tuple in
    let name_str =
      let s = Fmtc.to_to_string Name.pp name in
      if String.prefix ~pre:"$" s then
        (* Skolem vars may have a name incompatible with SMV so: *)
        "_" ^ s
      else s
    in
    let full_str =
      Format.sprintf "%s%s%a"
        name_str
        rel_sep
        Fmtc.(list ~sep:atom_sep Atom.pp) ats
    in 
    Symbol.make full_str
    (* keep trace of creations to allow to get original pairs back *)
    |> Fun.tap (fun hs -> HT.add names_and_tuples hs (name, tuple))

  
  let split sym =
    HT.get names_and_tuples sym
                                 
  let split_string str =
    split @@ Symbol.make str
   
end

module SMV_LTL = SMV.Make_SMV_LTL(SMV_atom)

module SMV_file_format = SMV.Make_SMV_file_format(SMV_LTL)

module Elo_to_SMV_LTL = Elo_to_LTL1.Make(SMV_LTL)

module Elo_to_SMV_model = Elo_to_model1.Make(SMV_LTL)(Elo_to_SMV_LTL)(SMV_file_format)

let pp = SMV_file_format.pp

let analyze = SMV_file_format.analyze
           
(* temporary *)
let run elo =
  Elo_to_SMV_model.run elo

let transfo = Transfo.make "to_smv1" run
