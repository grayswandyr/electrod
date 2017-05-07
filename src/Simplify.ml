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

    (* split multiple simultaneous bindings into many quantifications *)
    method visit_QAEN env quant sim_bindings blk = match sim_bindings with
      | [] -> assert false
      | [b] -> qaen quant sim_bindings blk
      | ((_, _, e) as b)::bs -> qaen quant [b] [fml e.exp_loc @@ self#visit_QAEN env quant bs blk]

    (* TODO rewrite QLO *)

    (* substitute let bindings *)
    method visit_Let env bindings fmls =
      (* substitute from right to left as a binding on the left may apply in the range of a binding on the right *)
      let subs = List.rev_map (function (Elo.BVar v, e) -> (v, e.prim_exp)) bindings in
      Elo.substitute#visit_prim_fml subs @@ block fmls

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
                  (fun arg r -> exp Location.dummy @@ rbinary arg join r) args call
      in
      self#visit_prim_exp env res.prim_exp
        
  end
  in
  walk#visit_t (ref []) f

let run elo =
  let open Elo in
  { elo with goals = List.map simplify_fml elo.goals }

let transfo = Transfo.make "simplify" run
