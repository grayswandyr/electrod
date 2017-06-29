open Containers
open GenGoal

module TS = TupleSet

(*******************************************************************************
 *  Simplify formulas: EXPECTED TO BE BE DONE AFTER CHECKING ARITIES
 *******************************************************************************)

(** Compared to Simplify1, this version maps qualified relations to formulas
    relying on cardinality arguments. *)


let fresh_var base exp =
  Var.fresh ~loc:exp.exp_loc base 

(* simplify Elo goals *)
class simplify = object (self : 'self)
  inherit Simplify1.simplify as super

  (* change relation qualifiers into formulas *)
  method visit_Qual env qual exp =
    Msg.debug (fun m -> m "Simplify2.visit_Qual <-- %a"
                          Elo.pp_prim_fml
                @@ GenGoal.qual qual exp);
    let prim_fml = match qual with
      | ROne ->
          icomp (iexp exp.exp_loc @@ card exp) ieq (iexp exp.exp_loc @@ num 1)
      | RLone ->
          icomp (iexp exp.exp_loc @@ card exp) lte (iexp exp.exp_loc @@ num 1)
      | RSome ->
          icomp (iexp exp.exp_loc @@ card exp) gte (iexp exp.exp_loc @@ num 1)
      | RNo ->
          icomp (iexp exp.exp_loc @@ card exp) ieq (iexp exp.exp_loc @@ num 0)
    in
    self#visit_prim_fml env prim_fml
    |> Fun.tap
    @@ fun res ->
    Msg.debug (fun m -> m "Simplify2.visit_Qual --> %a"
                          Elo.pp_prim_fml res)

end


let run elo =
  let open Elo in
  Msg.debug (fun m -> m "Entering Simplify2.simplify_fml");
  { elo with goal = (new simplify)#visit_t () elo.goal }
  

let transfo = Transfo.make "simplify2" run
