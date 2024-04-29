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

open Containers
module Map = Name.Map

type t = {
  decl : Relation.t Map.t;
  bitwidth : int;
}

let equal dom1 dom2 = (Map.equal Relation.equal dom1.decl dom2.decl) && dom1.bitwidth = dom2.bitwidth
let empty = {decl = Map.empty; bitwidth = 0;}

let mem name dom = Map.mem name dom.decl

let add name rel domain =
  assert (not @@ Map.mem name domain.decl);
  {
    decl = Map.add name rel domain.decl;
    bitwidth = domain.bitwidth;
  }

let get_exn name domain = Map.find name domain.decl
let get name domain = Map.get name domain.decl
let to_list domain= Map.to_list domain.decl
let of_list bw list= 
  {
    decl = Map.of_list list;
    bitwidth = bw;
  } 

let univ_atoms domain =
  let open Relation in
  let open Scope in
  match get_exn Name.univ domain with
  | Const { scope; _ } -> (
      match scope with Exact b -> b | Inexact _ -> assert false)
  | Var _ -> assert false
  | exception Not_found -> assert false

let pp out rels =
  Fmtc.(
    vbox
    @@ Map.pp ~pp_sep:(const string " ") ~pp_arrow:(const string " : ")
         ~pp_start:(const string "") ~pp_stop:(const string "")
         (styled `Cyan Name.pp)
         (Relation.pp ~print_name:false))
    out rels

let must name domain =
  assert (mem name domain);
  get_exn name domain |> Relation.scope |> Scope.must

let may name domain =
  assert (mem name domain);
  get_exn name domain |> Relation.scope |> Scope.may

let sup name domain =
  assert (mem name domain);
  get_exn name domain |> Relation.scope |> Scope.sup

let musts ?(with_univ_and_ident = true) domain =
  (if with_univ_and_ident then domain.decl
  else domain.decl |> Map.remove Name.univ |> Map.remove Name.iden)
  |> Map.map Relation.must |> Map.to_list

let arities domain = 
  let arities = Map.map Relation.arity domain.decl in
  Map.to_list arities

let update_domain_with_instance domain instance =
  let module R = Relation in
  let module I = Instance in
  let bw = domain.bitwidth in
  let relation_of_instance_item inst_item rel =
    assert (R.is_const rel);
    R.const (R.name rel) (R.arity rel) (Scope.exact inst_item)
  in
  let keep_instance __name = function
    | `Both (dom_entry, inst_entry) ->
        Some (relation_of_instance_item inst_entry dom_entry)
    | `Left dom_entry -> Some dom_entry
    | `Right _ ->
        (* cannot happen: Raw_to_ast checked that every
           instance is in the domain *)
        assert false
  in
  {
    decl = Map.merge_safe ~f:keep_instance domain.decl (Instance.to_map instance);
    bitwidth = bw;
  }


let rename atom_renaming name_renaming domain =
  let bitwidth = domain.bitwidth in
  to_list domain
  |> List.map (fun (name, rel) ->
         ( List.assoc ~eq:Name.equal name name_renaming,
           Relation.rename atom_renaming name_renaming rel ))
  |> (of_list bitwidth)

module P = Intf.Print.Mixin (struct
  type nonrec t = t

  let pp out {decl; _;}= pp out decl
end)

include P
