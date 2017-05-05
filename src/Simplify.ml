open Containers

open GenGoal


let fresh_var base exp =
  Var.fresh ~loc:exp.exp_loc base 

(* simplify Elo goals  *)
let simplify_fml f =
  let walk = object (self : 'self)
    inherit [_] GenGoal.map as super

    method visit_'v _ = Fun.id

    method visit_'i _ = Fun.id

    (* TODO split multiple simultaneous bindings into many quantifications *)

    (* TODO substitute let bindings *)
        
    (* change relation qualifiers into formulas *)
    method visit_Qual env qual exp =
      let prim_fml = match qual with
      | ROne -> qlo one [(Elo.bound_var @@ fresh_var "one" exp, exp)] [fml exp.exp_loc true_]
      | RLone -> qlo lone [(Elo.bound_var @@ fresh_var "lone" exp, exp)] [fml exp.exp_loc true_]
      | RSome -> qaen some [(false, [Elo.bound_var @@ fresh_var "some" exp], exp)] [fml exp.exp_loc true_]
      | RNo -> qaen no_ [(false, [Elo.bound_var @@ fresh_var "no" exp], exp)] [fml exp.exp_loc true_]
      in
      self#visit_prim_fml env prim_fml

    (* change box join in join *)
    method visit_BoxJoin env call args =
      let res = List.fold_right
                  (fun arg r -> exp (Location.span (arg.exp_loc, r.exp_loc))
                    @@ rbinary arg join r) args call
      in
      self#visit_prim_exp env res.prim_exp
      (* let _visitors_r0 = self#visit_exp env call  in *)
      (* let _visitors_r1 = self#visit_list self#visit_exp env args *)
      (* in *)
      (* BoxJoin (_visitors_r0, _visitors_r1) *)
  end
  in
  walk#visit_t [] f

let run elo =
  let open Elo in
  { elo with goals = List.map simplify_fml elo.goals }

let transfo = Transfo.make "simplify" run
