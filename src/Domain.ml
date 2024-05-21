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

let remove name domain = { domain with decls = Map.remove name domain.decls }
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
    @@ Map.pp ~pp_sep:Fmtc.hardline ~pp_arrow:(const string " : ")
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
let rec log2 n = if n < 2 then 0 else 1 + log2 (n asr 1)

let ceil_log2 nb =
  assert (nb >= 0);
  let log = log2 nb in
  (* computed log may be too small as there could be a non-null mantissa in a float representation *)
  if Int.(2 ** log < nb) then log + 1 else log

let%test _ = ceil_log2 0 = 0
let%test _ = ceil_log2 1 = 0
let%test _ = ceil_log2 2 = 1
let%test _ = ceil_log2 3 = 2
let%test _ = ceil_log2 4 = 2
let%test _ = ceil_log2 8 = 3
let%test _ = ceil_log2 9 = 4

let check_int_set univ_ts ints =
  let int_size = Tuple_set.size ints in
  let univ_size = Tuple_set.size univ_ts in
  let size_to_consider =
    if int_size <= 0 then
      (* in case the command if `for 0 Int`, the bitwidth must be large enough for the size of univ to be <= the max int. This would correspond to an interval large enough to represent the set [min_int .. 0 .. univ_size .. max_int]. So this at least 1 (for 0) + #univ (for positive values) + (#univ) (for negative values). *)
      (2 * univ_size) + 1
    else int_size
  in
  let bitwidth = ceil_log2 size_to_consider in
  let two_to_bitwidth_minus_1 = Int.(2 ** (bitwidth - 1)) in
  if
    int_size > 0
    && not
       @@ Iter.(
            for_all
              (fun nb -> Tuple_set.mem (Tuple.of_int nb) ints)
              Int.(~-two_to_bitwidth_minus_1 -- (two_to_bitwidth_minus_1 - 1)))
  then Msg.Fatal.incorrect_int_set (fun args -> args bitwidth ints);
  bitwidth

let compute_bitwidth univ_ts domain =
  match get Name.integers domain with
  | Some Relation.(Const { arity = 1; scope = Scope.Exact ints; _ }) ->
      { domain with bitwidth = check_int_set univ_ts ints }
      |> Fun.tap (fun { bitwidth; _ } ->
             Msg.debug (fun m ->
                 m "%a = %a, %a = %a --> bitwidth = %d" Name.pp Name.integers
                   Tuple_set.pp ints Name.pp Name.univ Tuple_set.pp univ_ts
                   bitwidth))
  | _ -> assert false

let ints domain =
  match get Name.integers domain with
  | Some Relation.(Const { arity = 1; scope = Scope.Exact ints; _ }) -> ints
  | _ -> assert false

let get_shift domain name =
  match get name domain with
  | Some Relation.(Const { arity = 3; scope = Scope.Exact triples; _ }) ->
      Some triples
  | _ -> None

let shl domain = get_shift domain Name.shl
let sha domain = get_shift domain Name.sha
let shr domain = get_shift domain Name.shr

module P = Intf.Print.Mixin (struct
  type nonrec t = t

  let pp out { decls; _ } = pp out decls
end)

include P
