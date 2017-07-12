(** Provides a converter from Electrod models to (part of) a solver model.  *)

open Containers

module Make
    (Ltl : Solver.LTL)
    (ConvertFormulas : Elo_to_LTL_intf.S
     with type ltl = Ltl.t and type atomic = Ltl.atomic)
    (Model : Solver.MODEL
     with type ltl = ConvertFormulas.ltl
      and type atomic = ConvertFormulas.atomic) =
struct

  (* Compute an LTL formula and the list of (constant) atomic
     propositions from a list of symmetries *)
  let syms_to_ltl (syms : Symmetry.t list) =
    let open Elo in
    let open Ltl in
    let sym_to_ltl (sym : Symmetry.t) =
      Symmetry.fold
        (fun (name1, tuple1) (name2, tuple2)
          ((atoms_acc, fml_acc) : atomic Sequence.t * Ltl.t) ->
          let at1 = make_atomic name1 tuple1 in
          let at_fml1 =  atomic at1 in
          let at2 = make_atomic name2 tuple2 in
          let at_fml2 = atomic at2 in
          (Sequence.cons at1 (Sequence.cons at2 atoms_acc),
           or_ (implies at_fml1 (lazy at_fml2))
             (lazy (and_ (iff at_fml1 at_fml2) (lazy fml_acc)))))
        sym
        (Sequence.empty, true_)
    in
    List.fold_left
      (fun (atoms_acc, fmls_acc) sym ->
         let (cur_atoms, cur_fml) = sym_to_ltl sym in
         (Sequence.append cur_atoms atoms_acc,
          Sequence.cons cur_fml fmls_acc))
      (Sequence.empty, Sequence.empty)
      syms

  
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
                         elo.instance } in
    
    (* TODO take care of the invariant too *)
    let goal_fml = match elo.goal with GenGoal.Run g | GenGoal.Check g -> g in
    let (rigid_goal, flexible, property) = ConvertFormulas.convert elo goal_fml in
    (* handling symmetries *)
    let (rigid_syms, syms_fmls) = syms_to_ltl elo.sym in
    let rigid = Sequence.append rigid_syms rigid_goal in
    Model.make ~rigid ~flexible ~invariant:syms_fmls ~property

end
