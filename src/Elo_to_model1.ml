(** Provides a converter from Electrod models to (part of) a solver model.  *)

open Containers

module S = Sequence

module Make
    (Ltl : Solver.LTL)
    (ConvertFormulas : Elo_to_LTL_intf.S
     with type ltl = Ltl.t and type atomic = Ltl.Atomic.t)
    (Model : Solver.MODEL
     with type ltl = ConvertFormulas.ltl
      and type atomic = ConvertFormulas.atomic) =
struct

  type atomic = Ltl.Atomic.t
  (* Compute an LTL formula and the list of atomic propositions from a
     list of symmetries *)
  let syms_to_ltl elo =
    let open Elo in
    let open Ltl in
    let syms = elo.sym in
    let dom = elo.domain in
    let sym_to_ltl (sym : Symmetry.t) =
      Symmetry.fold
        (fun (name1, tuple1) (name2, tuple2)
          ((rigid_atoms_acc, flex_atoms_acc, fml_acc)
           : atomic S.t *atomic S.t* Ltl.t)
          ->         
            (*We assume that a symmetry is well-formed: each pair of
              name and tuple (name, tuple) share the same name *)         
            if not (Name.equal name1 name2) then
              assert false
            else           
              let name_is_const =
                Domain.get_exn name1 dom |> Relation.is_const
              in
              let at1 = Ltl.Atomic.make name1 tuple1 in
              let at_fml1 = atomic at1 in
              let at2 = Ltl.Atomic.make name2 tuple2 in
              let at_fml2 = atomic at2 in
              if name_is_const then
                (S.cons at1 (S.cons at2 rigid_atoms_acc),
                 flex_atoms_acc,
                 or_ (implies at_fml1 (lazy at_fml2))
                   (lazy (and_ (iff at_fml1 at_fml2) (lazy fml_acc))))
              else
                (rigid_atoms_acc,
                 S.cons at1 (S.cons at2 flex_atoms_acc),
                 or_ (implies at_fml1 (lazy at_fml2))
                   (lazy (and_ (iff at_fml1 at_fml2) (lazy fml_acc))))
        )
        sym
        (S.empty, S.empty, true_)
    in
    List.fold_left
      (fun (rigid_atoms_acc, flex_atoms_acc, fmls_acc) sym ->
         let (cur_rigid_atoms, cur_flex_atoms, cur_fml) = sym_to_ltl sym in
         (S.append cur_rigid_atoms rigid_atoms_acc,
          S.append cur_flex_atoms flex_atoms_acc,
          S.cons ("-- (symmetry)", cur_fml) fmls_acc))
      (S.empty, S.empty, S.empty)
      syms

  (* Splits a list of formulas lf into two lists (invf, restf): the
     list of invar formulas and the list of the rest of the formulas. In
     case all the formulas in lf are invars, then the last formula of lf
     is put in restf.*)
  let split_invar_noninvar_fmls elo blk =
    let open Invar_computation in
    let (invf, restf) =
      List.partition_map
        (fun fml ->
           let color = ConvertFormulas.color elo fml in
           Msg.debug (fun m -> m
                                 "Color of formula %a : %a\n" Elo.pp_fml fml Invar_computation.pp color);
           match color with
             | Invar | Static_prop -> `Left (remove_always_to_invar fml)
             | _ -> `Right (fml)
        )
        blk
    in
    match (restf, List.rev invf) with
      | hd::tl , _ -> (invf, restf)
      | [] , hd::tl -> (tl, [add_always_to_invar hd])
      | _ -> assert false (*the goal cannot be empty*)


  (* From a non-empty list f1, f2, ..., fn of elo formulas, this
     function computes the elo formula "(f1 and ... and fn-1) implies not
     fn" *)
  let dualise_fmls fmls =
    let open GenGoal in
    match List.rev fmls with
      | [] -> assert false
      | hd::tl ->
          let premise = 
            List.fold_left
              (fun x y -> fml (Location.span (x.fml_loc, y.fml_loc))
                @@ lbinary x and_ y)
              (fml Location.dummy true_) tl
          in
          let rhs_fml =
            match hd.prim_fml with
              | LUn (Not, subfml) -> subfml
              | _ -> fml hd.fml_loc @@ lunary Not hd
          in
          fml premise.fml_loc @@
          lbinary premise Imp rhs_fml

  let run elo =
    let open Elo in
    (* #781 Handle instance:

       To handle the instance, one possibility would be to update the bound
       computation (bounds_exp) and [build_Ident].

       However, apparently, we won't need to differentiate the domain and the
       instance in the future. So we take the simpler path that consists in
       updating the domain itself. As this is confined to the following
       functions, we do this for the time being. If the need arises, a
       refactoring won't be too painful. *)
    let elo =
      Elo.{ elo with
              domain = Domain.update_domain_with_instance elo.domain
                         elo.instance;
              instance = Instance.empty } in

    Msg.debug (fun m ->
          m "Elo_to_model1.run: after instance update:@ %a"
            Elo.pp elo);

    (* walk through formulas, convert them to LTL and accumulate rigid
       and flexible variables. TODO: replace sequences by sets. *)
    (* let exception Early_stop in *)
    let translate_formulas fmls =
      (* try *)
      List.fold_left
        (fun (acc_r, acc_f, acc_fml) fml ->
           let (r, f, fml_str, ltl) = ConvertFormulas.convert elo fml in
           (* if ltl = Ltl.false_ then *)
           (*   raise Early_stop *)
           (* else *)
           (S.append r acc_r,
            S.append f acc_f,
            S.cons (fml_str, ltl) acc_fml))
        S.(empty, empty, empty) fmls
        (* with *)
        (*   Early_stop -> S.(empty, empty, Ltl.false_) *)
    in

    (* handling symmetries *)
    let (rigid_syms, flex_syms, syms_fmls) = syms_to_ltl elo in


    (* handling the goal *)
    let goal_blk = match elo.goal with GenGoal.Run g -> g in


    (* Partition the goal fmls into invars and non invars *)
    let detected_invars, general_fmls =
      split_invar_noninvar_fmls elo goal_blk
    in   
    Msg.debug (fun m -> m "Detected invariants : %a"
                          Elo.pp_block detected_invars);

    let spec_fml = dualise_fmls general_fmls in
    Msg.debug (fun m -> m "Elo property : %a" Elo.pp_fml spec_fml);

    let (rigid_goal, flex_goal, spec_fml_str, prop_ltl) =
      ConvertFormulas.convert elo spec_fml
    in

    (* handling invariants *)
    let (rigid_inv, flex_inv, invars) =
      translate_formulas @@ List.append detected_invars elo.Elo.invariants
    in


    let rigid = S.(append rigid_syms (append rigid_inv rigid_goal)) in
    let flexible = S.(append flex_syms (append flex_goal flex_inv)) in 
    Model.make ~elo ~rigid ~flexible
      ~invariant:S.(append invars syms_fmls) ~property:(spec_fml_str, prop_ltl)

end

