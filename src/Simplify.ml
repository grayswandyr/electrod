open Containers
open GenGoal

module TS = TupleSet

(*******************************************************************************
 *  Check arities #708
 *******************************************************************************)

(* computes the arity of a join *)
let join_arity ar1 ar2 = match ar1, ar2 with
  | Some a1, Some a2 ->
      let res = a1 + a2 - 2 in
      if res > 0 then Some res
      else None
  | Some _, None
  | None, Some _
  | None, None -> None

let str_exp =
  Fmtc.to_to_string (Fmtc.hbox2 @@ GenGoal.pp_exp Elo.pp_var Elo.pp_ident)

let check_arities elo =
  let open Elo in
  let open GenGoal in
  (* ctx is a map from identifiers to their arity  *)
  let rec walk_fml ctx { prim_fml; _ } =
    walk_prim_fml ctx prim_fml

  and walk_prim_fml ctx = function
    | True | False -> ()
    | Qual (ROne, exp)
    | Qual (RSome, exp) ->
        if arity_exp ctx exp = None then
          Msg.Fatal.arity_error
            (fun args -> args elo.file exp
              @@ Fmtc.strf
                   "enclosing formula is false as %s is always empty"
                   (str_exp exp))
    | Qual (_, exp) -> ignore @@ arity_exp ctx exp
    | RComp (e1, _, e2) -> 
        let ar1 = arity_exp ctx e1 in
        let ar2 = arity_exp ctx e2 in
        (if ar1 <> ar2 &&
            ar1 <> None &&
            ar2 <> None then
           Msg.Fatal.arity_error
             (fun args ->
                args elo.file e2
                  (Fmtc.strf "arity incompatible with that of %s" (str_exp e1))))
    | IComp (e1, op, e2) ->
        begin
          arity_iexp ctx e1;
          arity_iexp ctx e2
        end
    | LUn (_, fml) -> walk_fml ctx fml
    | LBin (f1, _, f2) -> 
        begin
          walk_fml ctx f1;
          walk_fml ctx f2
        end
    | QAEN (_, sim_bindings, blk) -> 
        let ctx = walk_sim_bindings ctx sim_bindings in
        walk_block ctx blk
    | QLO (_, bindings, blk) ->
        let ctx = walk_bindings ctx true bindings in
        walk_block ctx blk
    | Let (bindings, blk) -> 
        let ctx = walk_bindings ctx false bindings in
        walk_block ctx blk
    | FIte (c, t, e) ->
        walk_fml ctx c;
        walk_fml ctx t;
        walk_fml ctx e 
    | Block blk ->
        walk_block ctx blk

  and walk_block ctx blk =
    List.iter (walk_fml ctx) blk

  and walk_bindings ctx in_q = function
    | [] -> ctx
    | (BVar v, exp) :: bs ->
        let ar = arity_exp ctx exp in
        if in_q && ar <> Some 1 then (* under a quantification, range arity must be 1 *)
          Msg.Fatal.arity_error
            (fun args -> args elo.file exp "arity should be 1")
        else
          walk_bindings (ctx#add_variable v ar exp.must exp.sup) in_q bs

  and walk_sim_bindings ctx = function
    | [] -> ctx
    | sb :: sbs ->
        let ctx = walk_sim_binding ctx sb in
        walk_sim_bindings ctx sbs

  and walk_sim_binding ctx (_, vs, exp) =
    let ar = arity_exp ctx exp in
    if ar <> Some 1 then
      Msg.Fatal.arity_error (fun args -> args elo.file exp "arity should be 1")
    else
      let arity, must, sup = exp.arity, exp.must, exp.sup in
      List.fold_right
        (fun (BVar v) ctx -> ctx#add_variable v arity must sup) vs ctx

  and arity_exp ctx exp =
    (* if arity = Some 0 then the analysis has not been made yet, otherwise it has *)
    if Option.equal Int.equal exp.arity (Some 0) then
      match arity_prim_exp ctx exp with
        | Ok ar -> ar
        | Error msg -> Msg.Fatal.arity_error (fun args -> args elo.file exp msg)
    else
      exp.arity

  and update_exp exp arity must sup =
    exp.arity <- arity;
    exp.must <- must;
    exp.sup <- sup;
    exp.may <- TS.diff sup must;
    arity

  (* this function returns a [result] to factor the error messages out and also
     to enable to display the expression (i.e [exp], not [prim_exp]) concerned
     by the error*)
  (* IMPORTANT: the function receives an *expression*, not a *primitive* one; in
     order to easily set the mutable fields of the said expression. *)
  and arity_prim_exp ctx exp = match exp.prim_exp with
    | None_ ->
        Result.return @@ update_exp exp None TS.empty TS.empty
    | Univ -> 
        let arity, must, sup = ctx#get (Elo.Name Name.univ) in 
        Result.return @@ update_exp exp arity must sup
    | Iden ->
        let arity, must, sup = ctx#get (Elo.Name Name.iden) in 
        Result.return @@ update_exp exp arity must sup
    | Ident id -> 
        let arity, must, sup = ctx#get id in 
        Result.return @@ update_exp exp arity must sup
    | RUn (op, e) ->
        let ar = arity_exp ctx e in
        if ar <> Some 2 then
          Result.fail "arity should be 2"
        else
          Result.return
          @@ update_exp exp ar (TS.transpose e.must) (TS.transpose e.sup)
    | RBin (e1, op, e2) ->
        let ar1 = arity_exp ctx e1 in
        let ar2 = arity_exp ctx e2 in
        (match op with
          | Union when ar1 = ar2 || ar2 = None ->
              Result.return
              @@ update_exp exp ar1 (TS.union e1.must e2.must)
                   (TS.union e1.sup e2.sup)
          | Union when ar1 = None ->
              Result.return
              @@ update_exp exp ar2 (TS.union e1.must e2.must)
                   (TS.union e1.sup e2.sup)
          | Union ->
              Result.fail
                (Fmtc.strf "incompatible arities between %s and %s"
                   (str_exp e1)
                   (str_exp e2))
          | Diff when ar1 = None -> 
              Result.return
              @@ update_exp exp None TS.empty TS.empty
          | Diff when ar1 = ar2 || ar2 = None ->
              Result.return
              @@ update_exp exp ar1 (TS.diff e1.must e2.must)
                   (TS.diff e1.sup e2.sup)
          | Diff ->
              Result.fail
                (Fmtc.strf "incompatible arities between %s and %s"
                   (str_exp e1)
                   (str_exp e2))
          | Inter when ar1 = None || ar2 = None ->
              Result.return
              @@ update_exp exp None (TS.inter e1.must e2.must)
                   (TS.inter e1.sup e2.sup)
          | Inter when ar1 = ar2 -> 
              Result.return
              @@ update_exp exp ar1 (TS.inter e1.must e2.must)
                   (TS.inter e1.sup e2.sup)
          | Inter ->
              Result.fail
                (Fmtc.strf "incompatible arities between %s and %s"
                   (str_exp e1)
                   (str_exp e2))
          | Over when ar1 = ar2 ->
              if CCOpt.compare CCInt.compare ar1 (Some 1) <= 0 then
                Result.fail
                  (Fmtc.strf "arity of %s is < 2" (str_exp e1))
              else
                Result.return
                @@ update_exp exp ar1 (TS.override e1.must e2.must)
                     (TS.override e1.sup e2.sup)
          | Over when ar1 = None ->
              Result.return @@ update_exp exp ar2 e2.must e2.sup
          | Over when ar2 = None ->
              Result.return @@ update_exp exp ar1 e1.must e1.sup
          | Over ->
              Result.fail
                (Fmtc.strf "incompatible arities between %s and %s"
                   (str_exp e1)
                   (str_exp e2))
          | LProj when ar1 = None ->
              Result.return @@ update_exp exp None TS.empty TS.empty
          | LProj when ar1 = Some 1 ->
              Result.return @@ update_exp exp ar2 (TS.lproj e1.must e2.must)
                                 (TS.lproj e1.sup e2.sup)
          | LProj ->
              Result.fail "left projection should be on a set"
          | RProj when ar2 = None ->
              Result.return @@ update_exp exp None TS.empty TS.empty
          | RProj when ar2 = Some 1 ->
              Result.return @@ update_exp exp ar1 (TS.rproj e1.must e2.must)
                                 (TS.rproj e1.sup e2.sup)
          | RProj -> 
              Result.fail "right projection should be on a set"
          | Prod ->
              (match ar1, ar2 with
                | Some a1, Some a2 ->
                    let ar = Some (a1 + a2) in
                    Result.return
                    @@ update_exp exp ar (TS.product e1.must e2.must)
                         (TS.product e1.sup e2.sup)
                | None, _
                | _, None -> Result.return None)
          | Join ->
              let ar_join = join_arity ar1 ar2 in
              if ar_join = None then
                Result.fail @@
                Fmtc.strf "wrong arities for the dot join of %s and %s"
                  (str_exp e1) (str_exp e2)
              else
                Result.return
                @@ update_exp exp ar_join (TS.join e1.must e2.must)
                     (TS.join e1.sup e2.sup)
        )
    | RIte (c, t, e) -> 
        begin
          walk_fml ctx c;
          let a_t = arity_exp ctx t in
          let a_e = arity_exp ctx e in
          if Option.equal Int.equal a_t a_e then
            Result.return
            @@ update_exp exp a_t (TS.inter t.must e.must) (TS.union t.sup e.sup)
          else 
            Result.fail "incompatible arities in the bodies of 'then' and 'else'" 
        end
    | BoxJoin (call, args) ->
        (* build the iterated "plain" join to get arity/must/sup *)
        ignore @@ arity_exp ctx call;
        List.iter Fun.(ignore % arity_exp ctx) args;
        let res =
          List.fold_right
            (fun arg r ->
               GenGoal.exp
                 Location.(span (arg.exp_loc, r.exp_loc))
                 ~arity:Option.(return @@ get_exn arg.arity + get_exn r.arity - 2)
                 ~must:(TS.join arg.must r.must)
                 ~sup:(TS.join arg.sup r.sup)
               @@ rbinary arg join r
            ) args call
        in
        if res.arity = None || res.arity = Some 0 then
          Result.fail "wrong arities for the box join"
        else
          Result.return
          @@ update_exp exp res.arity res.must res.sup
    | Compr (sim_bindings, blk) ->
        let ctx2 = walk_sim_bindings ctx sim_bindings in
        begin
          walk_block ctx2 blk;
          (* Result.return *) (* accumulate lengths of variables in various bindings *)
          (* @@ Some List.( *)
          (*       fold_left (fun acc (_, vs, _) -> acc + length vs) 0 sim_bindings) *)
          match
            List.(flat_map (fun (_, vs, _) ->
                  map (fun (v) -> ctx2#get @@ var_ident_of_bound_var v) vs))
              sim_bindings
          with
            | [] -> assert false
            | (_, hd_must, hd_sup)::tl -> 
                let (ar, must, sup) =
                  List.fold_left
                    (fun (acc_ar, acc_must, acc_sup) (_, must, sup) ->
                       (acc_ar + 1,
                        TS.product acc_must must,
                        TS.product acc_sup sup))
                    (1, hd_must, hd_sup) tl
                in
                Result.return @@ update_exp exp (Some ar) must sup
        end
    | Prime e ->
        Result.return @@ update_exp exp e.arity e.must e.sup

  and arity_iexp ctx { prim_iexp; _ } =
    arity_prim_iexp ctx prim_iexp

  and arity_prim_iexp ctx = function
    | Num _ -> ()
    | Card exp -> ignore @@ arity_exp ctx exp
    | IUn (_, iexp) -> arity_iexp ctx iexp
    | IBin (iexp1, _, iexp2) -> 
        begin
          arity_iexp ctx iexp1;
          arity_iexp ctx iexp2
        end
  in
  let init = object 
    val variables = []

    val domain = elo.domain

    method add_variable v arity must sup =
      Msg.debug (fun m -> m "Simplify.add_variable %a %a %a %a"
                            Var.pp v
                            Fmtc.(option int) arity
                            TS.pp must
                            TS.pp sup);
      {< variables = (v, (arity, must, sup)) :: variables >}

    method get ident = match ident with
      | Elo.Tuple _ -> assert false (* no tuples yet, only present when instantiating *)
      | Var v -> List.Assoc.get_exn ~eq:Var.equal v variables
      | Name name ->
          let rel = Domain.get_exn name domain in
          Relation.(Some (arity rel), must rel, sup rel)      
  end
  in
  let walk_goal (Sat blk) =
    walk_block init blk
  in
  walk_goal elo.goal


(*******************************************************************************
 *  Simplify formulas: EXPECTED TO BE BE DONE AFTER CHECKING ARITIES
 *******************************************************************************)

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
      let prim_fml = match qual with
        | ROne ->
            icomp (iexp exp.exp_loc @@ card exp) ieq (iexp exp.exp_loc @@ num 1)
        | RLone ->
            icomp (iexp exp.exp_loc @@ card exp) lte (iexp exp.exp_loc @@ num 1)
        | RSome ->
            rcomp exp not_in
            @@ GenGoal.exp ~arity:None ~must:TS.empty ~sup:TS.empty exp.exp_loc
                 none
        | RNo ->
            rcomp exp in_
            @@ GenGoal.exp ~arity:None ~must:TS.empty ~sup:TS.empty exp.exp_loc
                 none
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
      Msg.debug (fun m -> m "Simplify.visit_BoxJoin --> %a"
                            (pp_prim_exp Elo.pp_var Elo.pp_ident)
                            res)


    method visit_Compr env sbs b =
      Msg.debug (fun m -> m "Simplify.visit_Compr <-- %a"
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
      Msg.debug (fun m -> m "Simplify.visit_Compr --> %a"
                            (pp_prim_exp Elo.pp_var Elo.pp_ident)
                            res)

  end
  in
  walk#visit_t env goal


let run elo =
  let open Elo in
  Msg.debug
    (fun m -> m "Entering Simplify.check_arities: <-- initial context =@ %a"
                Domain.pp elo.domain);
  check_arities elo;
  Msg.debug (fun m -> m "Simplify.check_arities -->@ %a" Elo.pp elo);
  Msg.debug (fun m -> m "Entering Simplify.simplify_fml");
  { elo with goal = simplify_fml elo elo.goal }
  

let transfo = Transfo.make "simplify" run
