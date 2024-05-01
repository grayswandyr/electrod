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

type t = { decls : Relation.t Map.t; bitwidth : int }

let equal dom1 dom2 =
  Map.equal Relation.equal dom1.decls dom2.decls
  && dom1.bitwidth = dom2.bitwidth

let empty = { decls = Map.empty; bitwidth = 0 }
let mem name dom = Map.mem name dom.decls

let add name rel domain =
  assert (not @@ Map.mem name domain.decls);
  { decls = Map.add name rel domain.decls; bitwidth = domain.bitwidth }

let get_exn name domain = Map.find name domain.decls
let get name domain = Map.get name domain.decls
let to_list domain = Map.to_list domain.decls
let of_list bw list = { decls = Map.of_list list; bitwidth = bw }

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
  (if with_univ_and_ident then domain.decls
  else domain.decls |> Map.remove Name.univ |> Map.remove Name.iden)
  |> Map.map Relation.must |> Map.to_list

let arities domain =
  let arities = Map.map Relation.arity domain.decls in
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
    decls =
      Map.merge_safe ~f:keep_instance domain.decls (Instance.to_map instance);
    bitwidth = bw;
  }

let rename atom_renaming name_renaming domain =
  let bitwidth = domain.bitwidth in
  to_list domain
  |> List.map (fun (name, rel) ->
         ( List.assoc ~eq:Name.equal name name_renaming,
           Relation.rename atom_renaming name_renaming rel ))
  |> of_list bitwidth

let bitwidth { bitwidth; _ } = bitwidth

let log2 n =
  if n < 0 then invalid_arg "log2: argument must be non-negative"
  else
    let rec loop n = if n <= 1 then 0 else 1 + loop (n asr 1) in
    loop n

let compute_bitwidth =
  let exception Int_problem in
  let tuple_of_int nb =
    Tuple.tuple1 @@ Atom.of_raw_ident
    @@ Raw_ident.ident (string_of_int nb) Lexing.dummy_pos Lexing.dummy_pos
  in
  let check_int_set ints =
    let bitwidth = log2 (Tuple_set.size ints) in
    if
      bitwidth = 0
      || bitwidth > 0
         && Iter.(
              for_all
                (fun nb -> Tuple_set.mem (tuple_of_int nb) ints)
                (~-(Int.pow 2 (bitwidth - 1)) -- (Int.pow 2 (bitwidth - 1) - 1)))
    then bitwidth
    else raise Int_problem
  in
  fun domain ->
    try
      match get Name.integers domain with
      | Some Relation.(Const { arity = 1; scope = Scope.Exact ints; _ }) ->
          { domain with bitwidth = check_int_set ints }
          |> Fun.tap (fun { bitwidth; _ } ->
                 Msg.info (fun m -> m "bitwidth = %d" bitwidth))
      | _ -> raise Int_problem
    with Int_problem -> Msg.Fatal.incorrect_int_set ()

let ints domain =
  match get Name.integers domain with
  | Some Relation.(Const { arity = 1; scope = Scope.Exact ints; _ }) -> ints
  | _ -> Msg.Fatal.incorrect_int_set ()

module P = Intf.Print.Mixin (struct
  type nonrec t = t

  let pp out { decls; _ } = pp out decls
end)

include P
