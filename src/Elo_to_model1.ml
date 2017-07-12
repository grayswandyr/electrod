(** Provides a converter from Electrod models to (part of) a solver model.  *)

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
       instance in the future. So we take the simpler path that consists in
       updating the domain itself. As this is confined to the following
       functions, we do this for the time being. If the need arises, a
       refactoring won't be too painful. *)
    let elo =
      Elo.{ elo with
              domain = Domain.update_domain_with_instance elo.domain
                         elo.instance } in
    (* TODO take care of the invariant and symmetries too *)
    let goal_fml = match elo.goal with GenGoal.Run g | GenGoal.Check g -> g in
    let (rigid, flexible, property) = ConvertFormulas.convert elo goal_fml in
    Model.make ~rigid ~flexible ~invariant:Sequence.empty ~property

end

