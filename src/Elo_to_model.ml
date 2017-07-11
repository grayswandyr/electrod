open Containers

module Make
    (ConvertFormulas : Elo_to_LTL_intf.S)
    (Model : Solver.MODEL
     with type ltl = ConvertFormulas.ltl
      and type atomic = ConvertFormulas.atomic) =
struct

  let run elo =
    let open Elo in
    (* #781 Handle instance:

       To handle the instance, one possibility would be to update the bound
       computation (bounds_exp) and [build_Ident].

       However, apparently, we won't need to differentiate the domain and the
       instance in the future. So we take the simpler path that consists in update
       the domain itself. As this is confined to the following functions, we do
       this for the time being. If the need arises, a refactoring won't be too
       painful. *)
    let elo =
      Elo.{ elo with
              domain = Domain.update_domain_with_instance elo.domain
                         elo.instance } in
    (* walk through formulas, convert them to LTL and accumulate rigid and
       flexible variables. TODO: replace sequences by sets. 

       TODO the [and_] below could make some variables disappear, which would
       still be present in the SMV model: actually, atoms should only be
       gathered on final LTL formulas! Here we address this with a special
       exception. *)
    let exception Early_stop in
    let convert_formulas fmls =
      (* try *)
        List.fold_left
          (fun (acc_r, acc_f, acc_fml) fml ->
             let (r, f, ltl) = ConvertFormulas.convert elo fml in
             (* if ltl = Ltl.false_ then *)
             (*   raise Early_stop *)
             (* else *)
               (Sequence.append r acc_r,
                Sequence.append f acc_f,
                Sequence.cons ltl acc_fml))
          Sequence.(empty, empty, empty) fmls
      (* with *)
      (*   Early_stop -> Sequence.(empty, empty, Ltl.false_) *)
    in
    (* TODO take care of the invariant and symmetries too *)
    let GenGoal.Sat goal_fmls = elo.goal in
    let (rigid, flexible, property) = convert_formulas goal_fmls in
    Model.make ~rigid ~flexible ~invariant:Sequence.empty ~property

end

