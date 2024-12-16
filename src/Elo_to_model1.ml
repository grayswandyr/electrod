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

(** Provides a converter from Electrod models to (part of) a solver
    model.  *)

open Containers
module S = Iter

module Make
    (Ltl : Solver.LTL)
    (ConvertFormulas : Elo_to_ltl_intf.S
                         with type ltl = Ltl.t
                          and type atomic = Ltl.Atomic.t)
    (Model : Solver.MODEL
               with type ltl = ConvertFormulas.ltl
                and type atomic = ConvertFormulas.atomic) =
struct
  type atomic = Ltl.Atomic.t

  (** Computes the LTL (the only temporal connective is "next") formula encoding
      the symmetry 
      in a single state only (int the initial state if symmetry_offset = 0) *)
  let single_state_sym_to_ltl symmetry_offset elo (sym : Symmetry.t) =
    let open Elo in
    let open Ltl in
    let sym_fml =
      Symmetry.fold
        (fun (name1, tuple1) (name2, tuple2) (fml_acc : Ltl.t) ->
          (*We assume that a symmetry is well-formed: each pair of
               name and tuple (name, tuple) share the same name *)
          if not (Name.equal name1 name2) then
            failwith
              Format.(
                sprintf "Badly-formed symmetry: %a != %a" Name.pp name1 Name.pp
                  name2)
          else
            let at1 = Ltl.Atomic.make elo.domain name1 tuple1 in
            let at_fml1 = atomic at1 in
            let at2 = Ltl.Atomic.make elo.domain name2 tuple2 in
            let at_fml2 = atomic at2 in
            and_
              (implies at_fml1 (lazy at_fml2))
              (lazy (implies (iff at_fml1 at_fml2) (lazy fml_acc))))
        sym true_
    in
    let rec iter_next_ltl k phi =
      if k = 0 then phi else iter_next_ltl (k - 1) (next phi)
    in
    iter_next_ltl symmetry_offset sym_fml

  (** Computes the full LTL formula encoding the symmetry in a temporal context *)
  let temporal_sym_to_ltl elo (sym : Symmetry.t) =
    let open Elo in
    let open Ltl in
    let all_equiv =
      Symmetry.fold
        (fun (name1, tuple1) (name2, tuple2) (fml_acc : Ltl.t) ->
          (*We assume that a symmetry is well-formed: each pair of
            name and tuple (name, tuple) share the same name *)
          if not (Name.equal name1 name2) then assert false
          else
            let at1 = Ltl.Atomic.make elo.domain name1 tuple1 in
            let at_fml1 = atomic at1 in
            let at2 = Ltl.Atomic.make elo.domain name2 tuple2 in
            let at_fml2 = atomic at2 in
            and_ (iff at_fml1 at_fml2) (lazy fml_acc))
        sym true_
    in
    always
    @@ implies
         (yesterday @@ historically all_equiv)
         (lazy (single_state_sym_to_ltl 0 elo sym))

  (* Computes an LTL formula for the whole list of symmetries.
     According to the value of the argument temporal_symmetry, the LTL formula
     either deals with the initial state or with the whole temporal trace. *)
  let syms_to_ltl temporal_symmetry symmetry_offset elo =
    let open Elo in
    let convert_sym =
      if temporal_symmetry then temporal_sym_to_ltl elo
      else single_state_sym_to_ltl symmetry_offset elo
    in
    List.fold_left
      (fun fmls_acc sym -> List.cons (convert_sym sym) fmls_acc)
      List.empty elo.sym

  let add_sym_comment_to_ltl_fml_list fml_list =
    List.fold_left
      (fun fmls_acc fml -> S.cons ("--  (symmetry)", fml) fmls_acc)
      S.empty fml_list

  (* Splits a list of formulas lf into four lists and a boolean (initf, invf,
     transf, restf, isInvarSpec): the list of init formulas, invar formulas, the
     list of trans formulas and the list of the rest of the
     formulas and a boolean saying whther restf is to check with LTLSPEC or INVARSPEC. 
     In case restf is empty, then the last formula of transf
     is put in restf.*)
  let split_invar_noninvar_fmls elo blk =
    let open Invar_computation in
    let invf, tmp_restf =
      List.partition_filter_map
        (fun fml ->
          let color = Invar_computation.color elo fml in
          (* Msg.debug (fun m ->
              m "Color of formula %a : %a\n"
              Elo.pp_fml fml Invar_computation.pp color); *)
          match color with
          | Invar | Static_prop -> `Left (remove_always_from_invar fml)
          | Init | Primed_prop | Trans | Temporal | Invar_spec -> `Right fml)
        blk
    in
    let transf, tmp_restf2 =
      List.partition_filter_map
        (fun fml ->
          let color = Invar_computation.color elo fml in
          (* Msg.debug (fun m ->
              m "Color of formula %a : %a\n"
              Elo.pp_fml fml Invar_computation.pp color); *)
          match color with
          | Trans -> `Left (remove_always_from_invar fml)
          | _ -> `Right fml)
        tmp_restf
    in
    let initf, restf =
      List.partition_filter_map
        (fun fml ->
          let color = Invar_computation.color elo fml in
          (* Msg.debug (fun m ->
              m "Color of formula %a : %a\n"
              Elo.pp_fml fml Invar_computation.pp color); *)
          match color with Init -> `Left fml | _ -> `Right fml)
        tmp_restf2
    in
    (*is_invar_spec is true if restf consits of one formula of the shape "F propsitional" or "not G propositional"*)
    let is_invar_spec =
      match restf with
      | [hd] -> 
        begin
          match Invar_computation.color elo hd with 
          | Invar_spec -> true
          | _ -> false
        end
      | _ -> false
    in
    match (restf, List.rev invf, List.rev transf, List.rev initf) with
    | _ :: _, _, _, _ -> (initf, invf, transf, restf, is_invar_spec)
    | [], hd :: tl, _, _ ->
        (initf, List.rev tl, transf, [ add_always_to_invar hd ], false)
    | [], _, hd :: tl, _ ->
        (initf, invf, List.rev tl, [ add_always_to_invar hd ], false)
    | [], _, _, hd :: tl -> (List.rev tl, invf, transf, [ hd ], false)
    | _ -> assert false

  (*the goal cannot be empty*)

  (* From a non-empty list f1, f2, ..., fn of elo formulas, this
     function computes the elo formula "(f1 and ... and fn-1) implies not
     fn".*)
  let dualise_fmls fmls =
    let open Elo in
    match List.rev fmls with
    | [] -> assert false
    | (Fml { node; _ } as hd) :: tl ->
      let rhs_fml =
        match node with LUn (Not, subfml) -> subfml | _ -> lunary not_ hd
      in
        match tl with 
        | [] -> rhs_fml
        | _ -> 
          let premise = List.fold_left (fun x y -> lbinary x and_ y) true_ tl in
          lbinary premise impl rhs_fml

  let run (elo, temporal_symmetry, symmetry_offset, single_formula) =
    let open Elo in
    let open Invar_computation in
    (* #781 Handle instance:

       To handle the instance, one possibility would be to update the bound
       computation (bounds_exp) and [build_Ident].

       However, apparently, we won't need to differentiate the domain and the
       instance in the future. So we take the simpler path that consists in
       updating the domain itself. As this is confined to the following
       functions, we do this for the time being. If the need arises, a
       refactoring won't be too painful. *)
    let elo =
      Elo.
        {
          elo with
          domain = Domain.update_domain_with_instance elo.domain elo.instance;
          instance = Instance.empty;
        }
    in
    Msg.debug (fun m ->
        m "Elo_to_model1.run: after instance update:@ %a" Elo.pp elo);
    (* walk through formulas, convert them to LTL and accumulate rigid
       and flexible variables. *)
    (* let exception Early_stop in *)
    let translate_formulas fmls =
      (* try *)
      List.fold_left
        (fun acc_fml fml ->
          let fml_str, ltl = ConvertFormulas.convert elo fml in
          (* if ltl = Ltl.false_ then *)
          (*   raise Early_stop *)
          (* else *)
          S.cons (fml_str, ltl) acc_fml)
        S.empty fmls
      (* with *)
      (*   Early_stop -> S.(empty, empty, Ltl.false_) *)
      |> S.rev
    in
    (* handling symmetries *)
    let sym_ltls = syms_to_ltl temporal_symmetry symmetry_offset elo in
    (* handling the goal *)
    let goal_blk = match elo.goal with Elo.Run (g, _) -> g in
    (* Partition the goal fmls into invars and non invars *)
    let detected_inits, detected_invars, detected_trans, general_fmls, is_invar_spec =
      if single_formula then
        (* the user wants only a single big LTL formula as a goal *)
        ([], [], [], goal_blk, false)
      else split_invar_noninvar_fmls elo goal_blk
    in

    (* Msg.debug (fun m ->
       m "Detected init : %a" (Elo.pp_block 0) detected_inits); *)

    (* Msg.debug (fun m ->
       m "Detected invariants : %a" Elo.pp_block detected_invars);*)

    (* Msg.debug (fun m ->
       m "Detected trans : %a" Elo.pp_block detected_trans); *)
    let gen_fmls = 
      if is_invar_spec then 
      begin
        match general_fmls with
        |[fml] -> [remove_eventually_from_invarspec fml]
        | _ -> assert false;
      end
      else general_fmls
    in
    Msg.debug (fun m ->
       m "Formula to chack before dualisation : %a" (Elo.pp_block 0) gen_fmls);
    let fml_prop = dualise_fmls gen_fmls in
    Msg.debug (fun m -> m "Formula to check after dualisation : %a" (Elo.pp_fml 0) fml_prop); 
    let fml_prop_comment, ltl_prop =
      let comment, p = ConvertFormulas.convert elo fml_prop in
      if temporal_symmetry || symmetry_offset > 0 || single_formula then
        ( "-- A symmetry breaking predicate is added at the beginning of the \
           LTLSPEC formula. This is either due to option --temporal-symmetry \
           or --single-formula.\n" ^ comment,
          Ltl.and_ (Ltl.conj sym_ltls) (lazy p) )
      else (comment, p)
    in
    (* handling init, invariants and trans *)
    let inits =
      if temporal_symmetry || symmetry_offset > 0 || single_formula then
        translate_formulas detected_inits
      else
        S.append
          (add_sym_comment_to_ltl_fml_list sym_ltls)
          (translate_formulas detected_inits)
    in
    let invars =
      translate_formulas @@ List.append detected_invars elo.Elo.invariants
    in
    let trans = translate_formulas detected_trans in
    Model.make ~elo ~init:inits ~invariant:invars ~trans
      ~property:(fml_prop_comment, ltl_prop) ~is_invar_spec
end
