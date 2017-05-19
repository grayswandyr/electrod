open Containers
open GenGoal

module TS = TupleSet

(*******************************************************************************
 *  Simplify formulas: EXPECTED TO BE BE DONE AFTER CHECKING ARITIES
 *******************************************************************************)

let fresh_var base exp =
  Var.fresh ~loc:exp.exp_loc base 

(* simplify Elo goals *)
class simplify = object (self : 'self)
  inherit [_] GenGoal.map as super

  (* the environment is not used, setting it here at [()] to avoid unbound type
     variables *)
  method visit_'v () = Fun.id

  method visit_'i _ = Fun.id

  (* split multiple simultaneous bindings into many quantifications *)
  method visit_QAEN env quant sim_bindings blk = 
    Msg.debug (fun m -> m "Simplify1.visit_QAEN <-- %a"
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
    Msg.debug (fun m -> m "Simplify1.visit_QAEN --> %a"
                          Elo.(pp_prim_fml pp_var pp_ident)
                          res);
    res

  (* TODO rewrite QLO *)

  (* substitute let bindings *)
  method visit_Let env bindings fmls =
    (* substitute from right to left as a binding on the left may apply in the
       range of a binding on the right *)
    Msg.debug (fun m -> m "Simplify1.visit_Let <-- %a"
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
    Msg.debug (fun m -> m "Simplify1.visit_Let --> %a"
                          (pp_prim_fml Elo.pp_var Elo.pp_ident) res)

  (* change relation qualifiers into formulas *)
  method visit_Qual env qual expr =
    Msg.debug (fun m -> m "Simplify1.visit_Qual <-- %a"
                          Elo.(pp_prim_fml pp_var pp_ident)
                @@ GenGoal.qual qual expr);
    let prim_fml = match qual with
      | ROne ->
          icomp (iexp expr.exp_loc @@ card expr) ieq (iexp expr.exp_loc @@ num 1)
      | RLone ->
          GenGoal.(lbinary (fml expr.exp_loc @@ qual rno expr)
                     or_ (fml expr.exp_loc @@ qual rone expr))
      | RSome ->
          rcomp expr not_in
          @@ GenGoal.exp ~arity:None ~must:TS.empty ~sup:TS.empty expr.exp_loc
               none
      | RNo ->
          rcomp expr in_
          @@ GenGoal.exp ~arity:None ~must:TS.empty ~sup:TS.empty expr.exp_loc
               none
    in
    self#visit_prim_fml env prim_fml
    |> Fun.tap
    @@ fun res ->
    Msg.debug (fun m -> m "Simplify1.visit_Qual --> %a"
                          (pp_prim_fml Elo.pp_var Elo.pp_ident) res)

  (* change box join in join *)
  method visit_BoxJoin env call args =
    Msg.debug (fun m -> m "Simplify1.visit_BoxJoin <-- %a[%a]"
                          Elo.(pp_exp pp_var pp_ident) call
                          Elo.(Fmtc.list @@ pp_exp pp_var pp_ident) args);
    let res =
      List.fold_right
        (fun arg r ->
           exp
             Location.(span (arg.exp_loc, r.exp_loc))
             ~arity:Option.(return @@ get_exn arg.arity + get_exn r.arity - 2)
             ~must:(TS.join arg.must r.must)
             ~sup:(TS.join arg.sup r.sup)
           @@ rbinary arg join r
        ) args call
    in
    self#visit_prim_exp env res.prim_exp
    |> Fun.tap
    @@ fun res ->
    Msg.debug (fun m -> m "Simplify1.visit_BoxJoin --> %a"
                          (pp_prim_exp Elo.pp_var Elo.pp_ident)
                          res)


  method visit_Compr env sbs b =
    Msg.debug (fun m -> m "Simplify1.visit_Compr <-- %a"
                          (pp_prim_exp Elo.pp_var Elo.pp_ident)
                          (GenGoal.compr sbs b));
    (* a function to compute all relevant pairs of disjoint variables for
       which a subformula will have to be generated. The function takes the
       product of [vs] with itself, except the diagonal and except pairs that
       are the transpose of the one being added (hence the [~eq] part). *)
    (* a utility function *)
    let make_var range v =
      exp ~arity:range.arity ~must:range.must ~sup:range.sup range.exp_loc
      @@ ident @@ Elo.var_ident_of_bound_var v
    in
    (* Given a set [vs] of variables, this function yields a list of formulas
       expressing that all these variables are as marked disjoint. *)
    let disj_fml_for_variables vs range =
      snd
      (* The following computes the product of the [vs] with temselves, except
         the diagonal and except symmetric pairs ('cause [disj x, y] <=> [disj
         y, x]).  Actually, the accumulator keeps the list of pairs of
         variables alreeady added, to check whether they have already been
         added, and the disequality formula (which really is what we're
         looking for.)  *)
      @@ List.fold_product
           (fun ((pairs, formulas) as acc) x y ->
              if Elo.equal_var x y
              || List.mem
                   ~eq:(fun (x, y) (a, b) ->
                         Pair.equal Elo.equal_var Elo.equal_var (x, y) (a, b)
                         ||
                         Pair.equal Elo.equal_var Elo.equal_var (x, y) (b, a))
                   (x, y) pairs
              then
                acc
              else
                ((x, y) :: pairs,
                 GenGoal.(fml Location.dummy
                          @@ rcomp (make_var range x) rneq (make_var range y)
                         ) :: formulas)
           )
           ([], []) vs vs
    in
    (* get the list of all disjointness formulas, for all disjoint parts *)
    let fmls =
      List.flat_map
        (fun (disj, vs, e) ->
           if disj then disj_fml_for_variables vs e else [])
        sbs
    in
    (* yield a new comprohension where the disequality predicates are added to
       the "such that" predicate *)
    compr
      (List.map
         (fun (_, vs, e) -> (false, vs, self#visit_exp env e)) sbs)
      (fmls @ List.map (self#visit_fml env) b)
    |> Fun.tap
    @@ fun res ->
    Msg.debug (fun m -> m "Simplify1.visit_Compr --> %a"
                          (pp_prim_exp Elo.pp_var Elo.pp_ident)
                          res)

end


let run elo =
  let open Elo in
  Msg.debug (fun m -> m "Entering Simplify1.simplify_fml");
  { elo with goal = (new simplify)#visit_t () elo.goal }
  

let transfo = Transfo.make "simplify1" run
