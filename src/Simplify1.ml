open Containers
open GenGoal

module TS = TupleSet

module L = Location

(*******************************************************************************
 *  Simplify formulas: EXPECTED TO BE BE DONE AFTER CHECKING ARITIES
 *******************************************************************************)

let fresh_var base exp =
  Var.fresh ~loc:exp.exp_loc base 

(* simplify Elo goals *)
class simplify = object (self : 'self)
  inherit [_] map as super

  (* the environment is not used, setting it here at [()] to avoid unbound type
     variables *)
  method visit_'v () = Fun.id

  method visit_'i _ = Fun.id

  method visit_Quant_One env q sim_bindings blk =
    (*re-write one x1,y1:r1, x2,y2:r2 | phi into
      some x1,y1:r1, x2,y2:r2 | (phi and all x1',x2':r1, x2',y2';r2 | ...)*)

    (* from the var list [x1, x2, ...] 
       create [x1', x2', ...] and the assoc list [(x1, x1'), (x2, x2') ...] 
       and the formula x1!=x1' or x2!=x2' or ...  *)
    let create_new_vars_and_assoc_list_and_comp_fml vs =
      List.fold_right
        (fun (Elo.BVar var) (new_vars, assoc, prim_fml) ->
           let new_var = Var.fresh_copy var in
           let new_var_as_ident = ident (Elo.var_ident new_var) in
           (Elo.bound_var new_var :: new_vars,
            CCList.Assoc.set ~eq:Var.equal var new_var_as_ident assoc,
            lbinary
              (fml L.dummy
               @@ rcomp
                    (exp L.dummy
                     @@ ident @@ Elo.var_ident var)
                    REq
                    (exp L.dummy new_var_as_ident)
              )
              and_
              (fml L.dummy prim_fml))
        )
        vs
        ([], [], true_)
    in

    let new_sim_bindings, assoc_new_vars, cmp_fml =
      List.fold_right
        (fun (disj, vars, e) (sim_bindings, acc_assoc, acc_fml) -> 
           let (new_vars, assoc, prim_fml) =
             create_new_vars_and_assoc_list_and_comp_fml vars in
           ((disj, new_vars, e) :: sim_bindings,
            assoc @ acc_assoc,
            lbinary (fml L.dummy prim_fml) and_ (fml L.dummy acc_fml)))
        sim_bindings
        ([], [], true_)
    in

    (* subst_blk = phi [x1'\x1, x2'\x2, ...] *)
    let subst_blk = Elo.substitute#visit_block assoc_new_vars blk in

    (* conversion of the block in a formula *)
    let fml_subst_blk =
      List.fold_right
        (fun curfml acc_fml -> fml L.dummy @@ lbinary curfml and_ acc_fml)
        subst_blk
        (fml L.dummy true_)
    in   

    let temp_forall_fml =
      quant all new_sim_bindings
        [fml L.dummy @@ lbinary fml_subst_blk impl (fml L.dummy cmp_fml)]
    in
    let temporary_fml =
      quant some sim_bindings 
        [fml L.dummy @@ lbinary
                          (fml L.dummy (block blk))
                          and_ (fml L.dummy temp_forall_fml) ]
    in
    self#visit_prim_fml env temporary_fml

  (* translate lone x:t|phi into one x:t|phi or no x:t|phi *)
  method visit_Quant_Lone env q sim_bindings blk =
    let res_fml =
      lbinary 
        (fml L.dummy @@ quant one sim_bindings blk)
        or_
        (fml L.dummy @@ quant no_ sim_bindings blk)
    in
    self#visit_prim_fml env res_fml

  (* split multiple simultaneous All/Some/No bindings into many quantifications *)
  method visit_Quant env q sim_bindings blk = 
    (* Msg.debug (fun m -> m "Simplify1.visit_Quant <-- %a" *)
    (*                       Elo.pp_prim_fml *)
    (*             @@ quant q sim_bindings blk); *)
    match q with
      | One -> self#visit_Quant_One env q sim_bindings blk
      | Lone -> self#visit_Quant_Lone env q sim_bindings blk
      | All | Some_ | No ->
          let res = match sim_bindings with
            | [] -> assert false
            | [b] ->
                let sim_bindings' =
                  List.map
                    (fun (disj, vs, e) -> (disj, vs, self#visit_exp env e))
                    sim_bindings in
                let blk' = List.map (self#visit_fml env) blk in
                quant q sim_bindings' blk'
            | ((_, _, e) as b)::bs ->
                quant q [b] [fml e.exp_loc @@ self#visit_Quant env q bs blk]
          in
          (* Msg.debug (fun m -> m "Simplify1.visit_Quant --> %a" *)
          (*                       Elo.pp_prim_fml *)
          (*                       res); *)
          res


  (* substitute let bindings *)
  method visit_Let env bindings fmls =
    (* substitute from right to left as a binding on the left may apply in the
       range of a binding on the right *)
    (* Msg.debug (fun m -> m "Simplify1.visit_Let <-- %a" *)
    (*                       Elo.pp_prim_fml *)
    (*             @@ let_ bindings fmls); *)
    List.fold_right
      (function (Elo.BVar v, e) ->
         Elo.substitute#visit_prim_fml [(v, e.prim_exp)])
      bindings
      (block fmls)
    |> self#visit_prim_fml env
    (* |> Fun.tap *)
    (* @@ fun res -> *)
    (* Msg.debug (fun m -> m "Simplify1.visit_Let --> %a" *)
    (*                       Elo.pp_prim_fml res) *)

  (* change relation qualifiers into formulas *)
  method visit_Qual env q expr =
    (* Msg.debug (fun m -> m "Simplify1.visit_Qual <-- %a" *)
    (*                       Elo.pp_prim_fml *)
    (*             @@ qual q expr); *)
    let prim_fml = match q with
      | ROne ->
          quant one
            [(false,
              [Elo.bound_var @@ fresh_var "one" expr],
              expr)]
            [fml expr.exp_loc true_]
      | RLone ->
          (lbinary (fml expr.exp_loc @@ qual rno expr)
             or_ (fml expr.exp_loc @@ qual rone expr))
      | RSome ->
          quant some
            [(false,
              [Elo.bound_var @@ fresh_var "some" expr],
              expr)]
            [fml expr.exp_loc true_]
      | RNo ->
          rcomp expr in_
          @@ exp ~arity:None expr.exp_loc none
    in
    self#visit_prim_fml env prim_fml
    (* |> Fun.tap *)
    (* @@ fun res -> *)
    (* Msg.debug (fun m -> m "Simplify1.visit_Qual --> %a" *)
    (*                       Elo.pp_prim_fml res) *)

  (* change box join in join *)
  method visit_BoxJoin env call args =
    (* Msg.debug (fun m -> m "Simplify1.visit_BoxJoin <-- %a[%a]" *)
    (*                       Elo.pp_exp call *)
    (*                       (Fmtc.list Elo.pp_exp) args); *)
    let res =
      List.fold_right
        (fun arg r ->
           exp
             L.(span (arg.exp_loc, r.exp_loc))
             ~arity:Option.(return @@ get_exn arg.arity + get_exn r.arity - 2)
           @@ rbinary arg join r
        ) args call
    in
    self#visit_prim_exp env res.prim_exp
    (* |> Fun.tap *)
    (* @@ fun res -> *)
    (* Msg.debug (fun m -> m "Simplify1.visit_BoxJoin --> %a" *)
    (*                       Elo.pp_prim_exp *)
    (*                       res) *)


  (* method visit_Compr env sbs b = *)
  (*   (\* Msg.debug (fun m -> m "Simplify1.visit_Compr <-- %a" *\) *)
  (*   (\*                       Elo.pp_prim_exp *\) *)
  (*   (\*                       (compr sbs b)); *\) *)
  (*   (\* a function to compute all relevant pairs of disjoint variables for *)
  (*      which a subformula will have to be generated. The function takes the *)
  (*      product of [vs] with itself, except the diagonal and except pairs that *)
  (*      are the transpose of the one being added (hence the [~eq] part). *\) *)
  (*   (\* a utility function *\) *)
  (*   let make_var range v = *)
  (*     exp ~arity:range.arity (\* ~must:range.must ~sup:range.sup *\) range.exp_loc *)
  (*     @@ ident @@ Elo.var_ident_of_bound_var v *)
  (*   in *)
  (*   (\* Given a set [vs] of variables, this function yields a list of formulas *)
  (*      expressing that all these variables are as marked disjoint. *\) *)
  (*   let disj_fml_for_variables vs range = *)
  (*     snd *)
  (*     (\* The following computes the product of the [vs] with temselves, except *)
  (*        the diagonal and except symmetric pairs ('cause [disj x, y] <=> [disj *)
  (*        y, x]).  Actually, the accumulator keeps the list of pairs of *)
  (*        variables alreeady added, to check whether they have already been *)
  (*        added, and the disequality formula (which really is what we're *)
  (*        looking for.)  *\) *)
  (*     @@ List.fold_product *)
  (*          (fun ((pairs, formulas) as acc) x y -> *)
  (*             if Elo.equal_var x y *)
  (*             || List.mem *)
  (*                  ~eq:(fun (x, y) (a, b) -> *)
  (*                        Pair.equal Elo.equal_var Elo.equal_var (x, y) (a, b) *)
  (*                        || *)
  (*                        Pair.equal Elo.equal_var Elo.equal_var (x, y) (b, a)) *)
  (*                  (x, y) pairs *)
  (*             then *)
  (*               acc *)
  (*             else *)
  (*               ((x, y) :: pairs, *)
  (*                (fml L.dummy *)
  (*                 @@ rcomp (make_var range x) rneq (make_var range y) *)
  (*                ) :: formulas) *)
  (*          ) *)
  (*          ([], []) vs vs *)
  (*   in *)
  (*   (\* get the list of all disjointness formulas, for all disjoint parts *\) *)
  (*   let fmls = *)
  (*     List.flat_map *)
  (*       (fun (disj, vs, e) -> *)
  (*          if disj then disj_fml_for_variables vs e else []) *)
  (*       sbs *)
  (*   in *)
  (*   (\* yield a new comprohension where the disequality predicates are added to *)
  (*      the "such that" predicate *\) *)
  (*   compr *)
  (*     (List.map *)
  (*        (fun (_, vs, e) -> (false, vs, self#visit_exp env e)) sbs) *)
  (*     (fmls @ List.map (self#visit_fml env) b) *)
  (*   (\* |> Fun.tap *\) *)
  (*   (\* @@ fun res -> *\) *)
  (*   (\* Msg.debug (fun m -> m "Simplify1.visit_Compr --> %a" *\) *)
  (*   (\*                       Elo.pp_prim_exp *\) *)
  (*   (\*                       res) *\) *)

end


let run elo =
  let open Elo in
  (* Msg.debug (fun m -> m "Entering Simplify1.simplify_fml"); *)
  { elo with goal = (new simplify)#visit_t () elo.goal }
  (* |> Fun.tap (fun _ -> Msg.debug (fun m -> m "Finished Simplify1.simplify_fml")) *)
  

let transfo = Transfo.make "simplify1" run
