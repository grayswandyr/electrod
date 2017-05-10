open Containers

open GenGoal


let fresh_var base exp =
  Var.fresh ~loc:exp.exp_loc base 

(* simplify Elo goals  *)
let simplify_fml env goal =
  let walk = object (self : 'self)
    inherit [_] GenGoal.map as super

    method visit_'v _ = Fun.id

    method visit_'i _ = Fun.id

    (* split multiple simultaneous bindings into many quantifications *)
    method visit_QAEN env quant sim_bindings blk = 
      Msg.debug (fun m -> m "Simplify.visit_QAEN <-- %a"
                            Elo.(pp_prim_fml pp_var pp_ident)
                  @@ qaen quant sim_bindings blk);
      let res = match sim_bindings with
        | [] -> assert false
        | [b] ->
            let sim_bindings' =
              List.map
                (fun (disj, vs, e) -> (disj, vs, self#visit_exp env e))
                sim_bindings in
            let blk' = List.map (self#visit_fml env) blk in
            qaen quant sim_bindings' blk'
        | ((_, _, e) as b)::bs ->
            qaen quant [b] [fml e.exp_loc @@ self#visit_QAEN env quant bs blk]
      in
      Msg.debug (fun m -> m "Simplify.visit_QAEN --> %a"
                            Elo.(pp_prim_fml pp_var pp_ident)
                            res);
      res

    (* TODO rewrite QLO *)

    (* substitute let bindings *)
    method visit_Let env bindings fmls =
      (* substitute from right to left as a binding on the left may apply in the
         range of a binding on the right *)
      Msg.debug (fun m -> m "Simplify.visit_Let <-- %a"
                            Elo.(pp_prim_fml pp_var pp_ident)
                  @@ let_ bindings fmls);
      List.fold_right
        (function (Elo.BVar v, e) ->
           Elo.substitute#visit_prim_fml [(v, e.prim_exp)])
        bindings
        (block fmls)
      |> self#visit_prim_fml env
      |> Fun.tap
      @@ fun res ->
      Msg.debug (fun m -> m "Simplify.visit_Let --> %a"
                            (pp_prim_fml Elo.pp_var Elo.pp_ident) res)

    (* change relation qualifiers into formulas *)
    method visit_Qual env qual exp =
      Msg.debug (fun m -> m "Simplify.visit_Qual <-- %a"
                            Elo.(pp_prim_fml pp_var pp_ident)
                  @@ GenGoal.qual qual exp);
      let arity = TupleSet.inferred_arity @@ Elo.sup env.Elo.domain exp in
      let make_bound_variables pattern e = (* [e] to get a location  *)
        (* create [arity] bound variables *)
        List.init arity (fun _ -> Elo.bound_var @@ fresh_var pattern exp)
      in
      let prim_fml = match qual with
        | ROne ->
            qlo one
              (List.map (fun v -> (v, exp)) @@ make_bound_variables "one" exp)
              [fml exp.exp_loc true_]
        | RLone ->
            qlo lone
              (List.map (fun v -> (v, exp)) @@ make_bound_variables "lone" exp)
              [fml exp.exp_loc true_]
        | RSome ->
            qaen some [(false, make_bound_variables "some" exp, exp)]
              [fml exp.exp_loc true_]
        | RNo ->
            qaen no_ [(false, make_bound_variables "no" exp, exp)]
              [fml exp.exp_loc true_]
      in
      self#visit_prim_fml env prim_fml
      |> Fun.tap
      @@ fun res ->
      Msg.debug (fun m -> m "Simplify.visit_Qual --> %a"
                            (pp_prim_fml Elo.pp_var Elo.pp_ident) res)

    (* change box join in join *)
    method visit_BoxJoin env call args =
      Msg.debug (fun m -> m "Simplify.visit_BoxJoin <-- %a[%a]"
                            Elo.(pp_exp pp_var pp_ident) call
                            Elo.(Fmtc.list @@ pp_exp pp_var pp_ident) args);
      let res =
        List.fold_right
          (fun arg r -> exp Location.dummy @@ rbinary arg join r) args call
      in
      self#visit_prim_exp env res.prim_exp
      |> Fun.tap
      @@ fun res ->
      Msg.debug (fun m -> m "Simplify.visit_BoxJoin --> %a"
                            (pp_prim_exp Elo.pp_var Elo.pp_ident) res)




  end
  in
  walk#visit_t env goal

let run elo =
  let open Elo in
  { elo with goals = List.map (simplify_fml elo) elo.goals }

let transfo = Transfo.make "simplify" run