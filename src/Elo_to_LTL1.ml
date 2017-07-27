(** Functor that provides a {!Elo_to_LTL_intf.S} converter given an
    implementation of LTL *)

open Containers

module G = GenGoal
module TS = TupleSet

let tuples_to_idents =
  List.map @@ fun tuple -> G.ident @@ Elo.tuple_ident tuple

module Make (Ltl : Solver.LTL) = struct
  open Ltl
  open Ltl.Infix

  type atomic = Ltl.atomic

  type ltl = Ltl.t

  type goal = Elo.goal

  (*****************************************************************
   * Bound computation
   ******************************************************************)
  type bounds = {
    must : TS.t;
    sup : TS.t;
    may : TS.t;
  }

  let make_bounds must sup =
    { must; sup; may = TS.diff sup must }

  let rec bounds_prim_exp domain pe =
    let open G in
    let open TS in
    match pe with         
      | BoxJoin (_,_) -> assert false (* SIMPLIFIED *)
      | Ident (Elo.Var v) ->          (* impossible: substituted *)
          failwith
          @@ Fmtc.strf "Elo_to_LTL1.bounds_prim_exp domain: \
                        %a should have been substituted"
               Var.pp v
      | Ident (Elo.Tuple t) ->
          let singleton = of_tuples [t] in
          make_bounds singleton singleton
      | Ident (Elo.Name n) -> 
          let rel = Domain.get_exn n domain in
          make_bounds (Relation.must rel) (Relation.sup rel)
      | None_  ->
          make_bounds empty empty
      | Univ  ->
          let univ = Domain.univ_atoms domain in
          make_bounds univ univ
      | Iden  ->
          let iden = Domain.get_exn Name.iden domain in
          make_bounds (Relation.must iden) (Relation.sup iden)
      | RUn (Transpose, e) ->
          let b = bounds_prim_exp domain e.prim_exp in
          make_bounds (transpose b.must) (transpose b.sup)
      | RUn (TClos, e) -> 
          let b = bounds_prim_exp domain e.prim_exp in
          make_bounds (transitive_closure b.must) (transitive_closure b.sup)
          |> Fun.tap
               (fun res -> 
                  Msg.debug (fun m ->
                        m
                          "Elo_to_LTL1.bounds_prim_exp(TClos):@\n\
                           must(%a) = %a@\nmay(%a) = %a"
                          Elo.pp_prim_exp pe
                          TS.pp res.must
                          Elo.pp_prim_exp pe
                          TS.pp res.may))
      | RUn (RTClos, e) -> 
          let iden = Domain.get_exn Name.iden domain in
          let b = bounds_prim_exp domain e.prim_exp in
          make_bounds (union b.must @@ Relation.must iden)
            (union b.sup @@ Relation.sup iden)
      | RBin (e1, Union ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (union b1.must b2.must) (union b1.sup b2.sup)
      | RBin (e1, Inter ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (inter b1.must b2.must) (inter b1.sup b2.sup)
      | RBin (e1, Over ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (override b1.must b2.must) (override b1.sup b2.sup)
      | RBin (e1, LProj ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (lproj b1.must b2.must) (lproj b1.sup b2.sup)
      | RBin (e1, RProj ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (rproj b1.must b2.must) (rproj b1.sup b2.sup)
      | RBin (e1, Prod ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (product b1.must b2.must) (product b1.sup b2.sup)
      | RBin (e1, Diff ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (diff b1.must b2.must) (diff b1.sup b2.sup)
      | RBin (e1, Join ,e2) -> 
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (join b1.must b2.must) (join b1.sup b2.sup)
      | RIte (_, e1, e2) ->
          let b1 = bounds_prim_exp domain e1.prim_exp in
          let b2 = bounds_prim_exp domain e2.prim_exp in
          make_bounds (inter b1.must b2.must) (union b1.sup b2.sup) 
      | Prime e ->
          bounds_prim_exp domain e.prim_exp
      | Compr (sim_bindings, _) ->
          let pmust =
            TS.of_tuples
            @@ bounds_sim_bindings (fun { must; _} -> must) domain [] sim_bindings
          in
          let psup =
            TS.of_tuples
            @@ bounds_sim_bindings (fun { sup; _} -> sup) domain [] sim_bindings
          in
          (* Msg.debug (fun m -> *)
          (*       m *)
          (*         "Elo_to_LTL1.bounds_exp(Compr):\ *)
                     (*          @\nmust(%a) = @[<hov>%a@]\ *)
                     (*          @\nsup(%a) = @[<hov>%a@]" *)
          (*         (Fmtc.(list ~sep:(const string ": ")) *)
          (*          @@ Elo.pp_sim_binding) *)
          (*         sim_bindings *)
          (*         TS.pp pmust *)
          (*         (Fmtc.(list ~sep:(const string ": ")) *)
          (*          @@ Elo.pp_sim_binding) *)
          (*         sim_bindings *)
          (*         TS.pp psup); *)
          make_bounds pmust psup


  (* The whole idea of this function (and the following auxiliary ones) is to
     apply the following scheme.  Say we want to compute: 

     must({x : s, y : x.t | ...})

     The result must be:

     must(s) -> { UNION_(tuple in must(s)) must( x.t [tuple/x] ) }

     as every pair in the comprehension is s.t. the range for y depends on the value for x.

     E.g.: Taking must(s) = { a b }, must(r) = { (a b) (b c) (a c) (a d) }:

     x : s, y : x.r

     should yield:

     (a x a.r) union (b x b.r) = {(a b) (a c) (a d) (b c)}

     So what we do is the following: Walk left-to-right along some sim_bindings
     and compute the corresponding bound, while accumulating already computed
     bounds. Finally everything is composed.  

     [subst] is a substitution from already-visited sim_bindings. [fbound] is the
     function returning the bound (say the must or the sup).  *)
  and bounds_sim_bindings
        fbound
        domain
        (subst : (Var.t, (Elo.var, Elo.ident) G.prim_exp) CCList.Assoc.t)
        (sbs : (Elo.var, Elo.ident) G.sim_binding list)
    : Tuple.t list = 
    let open List in
    match sbs with
      | [] -> assert false      (* nonempty list *)
      | [(disj, vs, r)] -> 
          (* compute the product of bounds for the head sim_binding, it yields a
             list of combinations (= lists) of tuples *)
          bounds_sim_binding fbound domain subst disj vs r
          (* as we're done, we concat tuples in every combination *)
          >|= Tuple.concat
      | (disj, vs, r)::tl ->
          (* compute the product of bounds for the head sim_binding, it yields a
             list of combinations (= lists) of tuples *)
          let hd_bnd =
            bounds_sim_binding fbound domain subst disj vs r
          in
          (* cast to create a substitution below *)
          let vars = map (fun (Elo.BVar v) -> v) vs in
          (* as explained above in the example: for every possible combination
             of tuples in [hd_bnd], we have to substitute *this* combination in
             the tail, then compute the resulting product and add it to the
             whole possibilities *)
          fold_left
            (fun acc combination ->
               acc
               @ (* we use @ instead of union because it should be faster and
                    the end result will be converted back into a set anyway *)
               product_for_one_hd_combination
                 fbound domain subst vars tl combination)
            []                  (* empty accumulator *)
            hd_bnd

  (* given the list of tuples corresponding to the bound for the head sim
     binding (so it's a list of tuples, as there are multiple variables bound
     to a given range), compute all the products with all the results obtained
     by substituting in the tail according to the substitutions induced by the
     said head tuples *)
  and product_for_one_hd_combination fbound domain subst vars
        (tail : (Elo.var, Elo.ident) G.sim_binding list)
        (hd_combination : Tuple.t list)
    : Tuple.t list =
    let open List in
    (* compute the corresponding substitution for this instance of the head bound *)
    let subst2 = combine vars (tuples_to_idents hd_combination) @ subst in
    (* compute all the tuples for the tail with *this* subtitution *)
    let tl_bnd =
      bounds_sim_bindings fbound domain subst2 tail
    in
    (* create all combinations with this instance of the head bound and the
       tuples computed for the tail *)
    product Tuple.(@@@) [Tuple.concat hd_combination] tl_bnd

  (* for one sim_binding *)
  and bounds_sim_binding 
        fbound
        domain
        (subst : (Var.t, (Elo.var, Elo.ident) G.prim_exp) CCList.Assoc.t) 
        disj
        vars
        (dom : (Elo.var, Elo.ident) G.exp)
    : Tuple.t list list
    =
    let open List in
    (* compute the range after substitution w/ previous bindings in the telescope *)
    let range' = Elo.substitute#visit_exp subst dom in
    (* compute the bound for the range of *this* sim binding. NOTE: [range_bnd]
       is given as a [Tuple.t list] instead of a [TS.t] for technical purposes
       only. As a consequence, the returned value for the whole function is a
       set of sets of tuples represented by a list of lists... *)
    let range_bnd =
      TS.to_list @@ fbound @@ bounds_prim_exp domain range'.G.prim_exp
    in
    (* compute the bound for the whole head sim binding  *)
    let lg = length vars in
    let prod =
      (* create as many copies as necessary (= nb of variables) of the domain *)
      init lg (fun _ -> range_bnd)
      (* & compute product (NOTE: tuples are not concatenated, just put in the
         same list representing a combination of tuples).  

         E.g.: suppose `range_bnd = [(1, 2); (3, 4)]`. Then we obtain, for `lg = 3`:

         [
         [(3, 4); (3, 4); (3, 4)]; 
         [(3, 4); (3, 4); (1, 2)]; 
         [(3, 4); (1, 2); (3, 4)];
         [(3, 4); (1, 2); (1, 2)]; 
         [(1, 2); (3, 4); (3, 4)]; 
         [(1, 2); (3, 4); (1, 2)];
         [(1, 2); (1, 2); (3, 4)]; 
         [(1, 2); (1, 2); (1, 2)]
         ] *)
      |> cartesian_product 
    in
    (* Remove lines where there are tuples in common if [disj = true].
       [all_different] is defined below. *)
    (if disj then filter all_different prod else prod)
  (* |> Fun.tap *)
  (* @@ fun res -> *)
  (* Msg.debug *)
  (*   (fun m -> *)
  (*      m "bounds_sim_binding %B %a %a --> @[<hov>%a@]" *)
  (*        disj *)
  (*        Fmtc.(list ~sep:(const string ", ")@@ Elo.pp_var) vars *)
  (*        Fmtc.(list ~sep:sp @@ Tuple.pp) range_bnd *)
  (*        Fmtc.(brackets @@ list ~sep:sp @@ brackets @@ list ~sep:sp @@ Tuple.pp) res) *)


  (* says whether all tuples in a list are diffferent form one another *)
  and all_different (tuples : Tuple.t list) : bool = match tuples with
    | [] -> true
    | hd::tl when List.mem ~eq:Tuple.equal hd tl -> false
    | _::tl -> all_different tl



  (***************************************************************** 
   * Semantic function
   ***************************************************************************************)


  (* FIRST: some functions used for the semantics of a transitive closure. *)

  (* given a 2-tuple set ts, this function computes the domain and the
     co-domain of ts, i.e., the set (sequence) of atoms that are the
     first elements of a 2-tuple in ts, and the set (sequence) of atoms
     thare are the second elements of a 2-tuple in ts *)
  let compute_domain_codomain ts =
    assert (TS.inferred_arity ts = 2);
    let open Sequence in
    let s = TS.to_seq ts in
    let split_seq (s1_acc, s2_acc) tup =
      (cons (Tuple.ith 0 tup) s1_acc,
       cons (Tuple.ith 1 tup) s2_acc)
    in
    fold split_seq (empty, empty) s

  (* given a 2-tuple set, this function computes the maximum length of
     a path (x1, ... xn) such that each 2-tuple (xi, xi+1) is in the
     tuple set.  Used to compute the number of iterations needed for
     transitive closure term. *)
  let compute_tc_length ts =
    (* Printf.printf "arity of relation : %d\n" (TS.inferred_arity ts); *)    
    assert (TS.inferred_arity ts = 2);
    let open Sequence in
    let s1, s2 = compute_domain_codomain ts in    
    let core_ats = inter ~eq:Atom.equal s1 s2 in
    let core_length = (length core_ats) - 1  in
    (* is it possible that x1 is not in the core (intersection of the
       domain and the codomain) ? *)
    let first_elt_in_core = subset ~eq:Atom.equal s1 core_ats in

    (* is it possible that xn is not in the core (intersection of the
       domain and the codomain) ? *)
    let last_elt_in_core = subset ~eq:Atom.equal s2 core_ats in

    match first_elt_in_core, last_elt_in_core with
      | true, true -> core_length
      | false, false -> core_length + 2
      | _ -> core_length + 1 


  (* computes the transitive closure of the term acc_term by k iterative
     squares (t+t.t)+(t+t.t)(t+t.t) + ... *)

  let rec iter_squares (acc_term : (Elo.var, Elo.ident) G.exp) k =
    let open Location in
    match k with
      | 0 -> G.(exp None dummy none)
      | 1 -> acc_term
      | _ ->
          let new_exp =
            G.(exp acc_term.arity dummy
               @@ rbinary acc_term union
                    (exp acc_term.arity dummy @@ rbinary acc_term join acc_term))
          in
          iter_squares new_exp (max (k lsr 1) ((k+1) lsr 1))

  (* computes the transitive closure of the term t by k joins
     (alternative to iter_squares) t + t.t + t.t.t + ... *)

  let iter_tc (t : (Elo.var, Elo.ident) G.exp) k =
    let open Location in
    let t_to_the_k = ref t in
    let tc = ref t in
    for i=2 to k do
      t_to_the_k := G.(exp t.arity dummy @@ rbinary !t_to_the_k join t);
      tc := G.(exp t.arity dummy @@ rbinary !tc union !t_to_the_k);
    done;
    !tc


  (* utility function for build_Join *)
  let eligible_pairs (tuple, lazy r_sup, lazy s_sup) = 
    let open List in
    fold_left (fun acc b ->
          filter_map
            (fun c ->
               if Tuple.is_in_join tuple b c then
                 Some (b, c)
               else
                 None) s_sup
          |> rev_append acc) empty r_sup
    |> to_seq

  (* let eligible_pairs = *)
  (*   CCCache.(with_cache *)
  (*              (lru ~eq:(fun (t1, r1, s1, _, _) (t2, r2, s2, _, _) -> *)
  (*                     Tuple.equal t1 t2 *)
  (*                     && Elo.equal_exp r1 r2 *)
  (*                     && Elo.equal_exp s1 s2) *)
  (*                  256) *)
  (*              eligible_pairs) *)


  class ['env] converter = object (self : 'self)
    inherit ['self] GenGoalRecursor.recursor as super

    method visit_'v (env : 'env) = Fun.id

    method visit_'i (env : 'env) = Fun.id

    (* fml  *)                        

    method build_fml (env : 'env) _ ltl _ = ltl

    method build_Run (env : 'env) _ = Fun.id

    method build_True (env : 'env) = true_

    method build_False (env : 'env) = false_

    method build_Block (env : 'env) _ = conj

    method build_FIte (env : 'env) _ _ _ = ifthenelse 

    method build_Let (env : 'env) bs block bs' block'= assert false (* SIMPLIFIED *)

    (* quant *)

    (* re-defining this method to avoid going down in the block as a
       substitution must be made first *)
    method visit_Quant env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_quant env _visitors_c0  in
      let _visitors_r1 =
        self#visit_list self#visit_sim_binding env _visitors_c1  in
      let _visitors_r2 = [true_]  in
      self#build_Quant env _visitors_c0 _visitors_c1 _visitors_c2
        _visitors_r0 _visitors_r1 _visitors_r2

    method build_Quant (env : 'env) quant sim_bindings blk _ sim_bindings' _ =
      Msg.debug
        (fun m -> m "build_Quant <-- %a"
                    (Elo.pp_prim_fml)
                    (G.quant quant sim_bindings blk)
        );
      match quant with
        | G.Lone | G.One ->
            assert false        (* SIMPLIFIED *)
        | G.All | G.Some_ | G.No ->
            assert (List.length sim_bindings = 1); (* SIMPLIFIED *)
            let disj, xs, s = List.hd sim_bindings in
            let _, _, s' = List.hd sim_bindings' in
            let tuples_of_sim_binding ~disj (vars : Elo.var list) (dom : Tuple.t list) =
              let open List in
              (* (\* substituitons take [Var.t] keys *\) *)
              (* let xs = map (fun (Elo.BVar v) -> v) vars in *)
              (* let tuples_as_idents = map (fun t1 -> G.ident @@ Elo.Tuple t1) dom in *)
              let lg = length vars in
              (* create as many copies as necessary (= nb of variables) of the domain *)
              init lg (fun _ -> dom)
              (* take their cartesian product *)
              |> cartesian_product
              (* remove lines where there are tuples in common if [disj = true] *)
              |> (if disj then
                    filter (fun l -> length l = length @@ sort_uniq l) else Fun.id)
              (* |> Fun.tap (fun res -> Msg.debug (fun m-> *)
              (*       m "tuples_of_sim_binding (disj:%B) vars:%a domain:%a@\n  -->@ %a " *)
              (*         disj *)
              (*         Fmtc.(brackets @@ list ~sep:sp Elo.pp_var) vars *)
              (*         Fmtc.(braces @@ list ~sep:comma Tuple.pp) dom *)
              (*         Fmtc.(vbox @@ list ~sep:cut *)
              (*               @@ hvbox2 @@ brackets *)
              (*               @@ list ~sep:comma Tuple.pp) res *)
              (*     )) *)
              |> to_seq
            in
            let sub_for tuples =
              let tuples_as_idents =
                List.map (fun t1 -> G.ident @@ Elo.Tuple t1) tuples in
              let xs_as_vars = List.map (fun (Elo.BVar v) -> v) xs in
              (* we zip the bound variables and the 1-tuples to get a list of
                 substitutions *)
              List.combine xs_as_vars tuples_as_idents
              (* |> Fun.tap (fun res -> *)
              (*       Msg.debug (fun m -> *)
              (*             m "sub_for %a = %a" *)
              (*               (Fmtc.(list Tuple.pp)) tuples *)
              (*               (Fmtc.(brackets *)
              (*                      @@ list *)
              (*                      @@ pair ~sep:(const string "→") Var.pp *)
              (*                      @@ Elo.pp_prim_exp)) res *)
              (*           )) *)
            in

            (* [pos_or_neg] tells whether the quantifier was a [no ...], in
               which case we consider the whole as [all ... | not ...]. [link]
               tells how to connect a premise and a test in the may part of the
               formula. *)
            let (bigop, smallop, link, pos_or_neg) = match quant with
              | G.All -> (wedge, and_, implies, Fun.id)
              | G.Some_ -> (vee, or_, and_, Fun.id)
              | G.No -> (wedge, and_, implies, not_)
              | G.Lone | G.One -> assert false (* SIMPLIFIED *)
            in
            let { must; may; _ } = env#must_may_sup s in
            let sem_of_substituted_blk tuples = 
              lazy (pos_or_neg
                    @@ (self#visit_prim_fml env) (* [[...]] *)
                    @@ (Elo.substitute#visit_prim_fml
                          (sub_for tuples)
                          (G.block blk)
                          (* |> Fun.tap (fun s -> *)
                          (*       Msg.debug (fun m -> *)
                          (*             m "%a[%a] -->@   %a" *)
                          (*               Elo.pp_block blk *)
                          (*               Fmtc.(list @@ pair ~sep:(const string " := ") *)
                          (*                               Var.pp Elo.pp_prim_exp) *)
                          (*               (sub_for tuples) *)
                          (*               Elo.pp_prim_fml s)) *)
                       )) (* blk [tuples / xs] *)
            in
            Msg.debug (fun m ->
                  m "build_Quant: ENTERING MUSTPART" );
            let mustpart =
              bigop
                ~range:(tuples_of_sim_binding ~disj xs @@ TS.to_list must)
                (fun tuples -> sem_of_substituted_blk tuples)
            in
            Msg.debug (fun m ->
                  m "build_Quant: must(%a) = %a@\nmustpart = %a@\nENTERING MAYPART"
                    (Elo.pp_exp) s
                    TS.pp must
                    Ltl.pp mustpart);
            let maypart =
              lazy 
                (bigop
                   ~range:(tuples_of_sim_binding ~disj xs @@ TS.to_list may)
                   (fun tuples ->
                      (* if several variables were bound to the same range, then
                         we must apply the characteristic function thereof to
                         every candidate tuples for these variables; and then
                         take the conjunction. Note: if several variables range
                         in the same set, then we will apply the characteristic
                         function many times to the same tuples: as an
                         optimization, we keep --only in the computation of the
                         premise-- only unique tuples to avoid this superfluous
                         repetition. *)
                      let premise = 
                        wedge
                          List.(to_seq @@ sort_uniq ~cmp:Tuple.compare tuples)
                          (fun tuple -> lazy (s' tuple))
                      in
                      (* Msg.debug (fun m -> m "(build_Quant.premise) %a" Ltl.pp premise); *)
                      lazy (link premise @@ sem_of_substituted_blk tuples)
                   ))
            in
            Msg.debug (fun m ->
                  m "build_Quant: may(%a) = %a@\nmaypart = %a"
                    (Elo.pp_exp) s
                    TS.pp may
                    Ltl.pp (Lazy.force maypart));
            (smallop mustpart maypart)
            |> Fun.tap (fun res ->
                  Msg.debug (fun m ->
                        m "build_Quant [[%a %a %a]] -->@ %a"
                          G.pp_quant quant
                          (Fmtc.(list ~sep:comma) @@ Elo.pp_sim_binding) sim_bindings
                          Elo.pp_block blk
                          Ltl.pp res
                      ))

    method build_One (env : 'env) = G.One

    method build_Lone (env : 'env) = G.Lone

    method build_All (env : 'env) = G.All

    method build_No (env : 'env) = G.No 

    method build_Some_ (env : 'env) = G.Some_    

    (* lbinop *)      

    method build_LBin (env : 'env) f1 op f2 f1' op' f2' =
      op' f1 f2 f1' f2'
      (* |> Fun.tap (fun res -> *)
      (*       Msg.debug (fun m -> *)
      (*             m "build_LBin [[%a %a %a]] -->@ %a" *)
      (*               Elo.pp_fml f1 *)
      (*               G.pp_lbinop op *)
      (*               Elo.pp_fml f2 *)
      (*               Ltl.pp res *)
      (*           )) *)

    method build_And (env : 'env) _ _ = fun a b -> and_ a (lazy b)

    method build_Iff (env : 'env) _ _ = iff

    method build_Imp (env : 'env) _ _ = fun a b -> implies a (lazy b)

    method build_U (env : 'env) _ _ = until

    method build_Or (env : 'env) _ _ = fun a b -> or_ a (lazy b)

    method build_R (env : 'env) _ _ = releases

    method build_S (env : 'env) _ _ = since

    (* lunop *)                     

    method build_LUn (env : 'env) op f op' f' =
      op' f f'
      (* |> Fun.tap (fun res -> *)
      (*       Msg.debug (fun m -> *)
      (*             m "[[%a %a]] -->@ %a" *)
      (*               G.pp_lunop op *)
      (*               Elo.pp_fml f *)
      (*               Ltl.pp res *)
      (*           )) *)

    method build_X (env : 'env) _ = next

    method build_F (env : 'env) _ = eventually

    method build_G (env : 'env) _ = always

    method build_H (env : 'env) _ = historically

    method build_O (env : 'env) _ = once

    method build_P (env : 'env) _ = yesterday

    method build_Not (env : 'env) _ = not_

    (* compo_op *)

    method build_RComp (env : 'env) f1 op f2 f1' op' f2' =
      op' f1 f2 f1' f2'
      (* |> Fun.tap (fun res -> *)
      (*       Msg.debug  *)
      (*         (fun m -> m "build_RComp [[%a %a %a]] --> %a" *)
      (*                     Elo.pp_exp f1 *)
      (*                     G.pp_comp_op op *)
      (*                     Elo.pp_exp f2 *)
      (*                     Ltl.pp res) *)
      (*     ) *)


    (* method build_REq (env : 'env) r s r' s' = *)
    (*   self#build_In env r s r' s' +&& lazy (self#build_In env s r s' r') *)

    method build_REq (env : 'env) r s r' s' =
      let r_bounds = env#must_may_sup r in
      let s_bounds = env#must_may_sup s in
      let inter = TS.inter r_bounds.may s_bounds.may in
      wedge ~range:(TS.to_seq r_bounds.must) (fun t -> lazy (s' t))
      +&& lazy (wedge ~range:(TS.to_seq s_bounds.must) (fun t -> lazy (r' t)))
      +&& lazy (wedge ~range:(TS.to_seq inter) (fun bs -> lazy (r' bs @<=> s' bs)))
      +&& lazy (wedge ~range:(TS.to_seq @@ TS.diff r_bounds.may inter)
                  (fun bs -> lazy (r' bs @=> lazy (s' bs))))
      +&& lazy (wedge ~range:(TS.to_seq @@ TS.diff s_bounds.may inter)
                  (fun bs -> lazy (s' bs @=> lazy (r' bs))))

    method build_In (env : 'env) r s r' s' =
      let { must; may; _} = env#must_may_sup r in
      (* Msg.debug (fun m -> m "build_In: %a in %a@\nmust(%a) = %a@\nmay(%a) = %a" *)
      (*                       Elo.pp_exp r *)
      (*                       Elo.pp_exp s *)
      (*                       Elo.pp_exp r *)
      (*                       TS.pp must *)
      (*                       Elo.pp_exp r *)
      (*                       TS.pp may *)
      (*           ); *)
      wedge ~range:(TS.to_seq must) (fun t -> lazy (s' t))
      +&& lazy (wedge ~range:(TS.to_seq may)
                  (fun bs -> lazy (r' bs @=> lazy (s' bs))))

    method build_NotIn (env : 'env) r s r' s' =
      not_ @@ self#build_In env r s r' s'

    method build_RNEq (env : 'env) r s r' s' =
      not_ @@ self#build_REq env r s r' s'

    (* icomp_op *)

    method build_IComp (env : 'env) e1 _ e2 e1_r op e2_r = op e1_r e2_r

    method build_Gt (env : 'env) = comp gt

    method build_Gte (env : 'env) = comp gte

    method build_IEq (env : 'env) = comp eq

    method build_INEq (env : 'env) = comp neq

    method build_Lt (env : 'env) = comp lt

    method build_Lte (env : 'env) = comp lte

    (* rqualify *)

    method build_Qual (env : 'env) _ r q' r' = assert false (* SIMPLIFIED *)

    method build_RLone (env : 'env) = G.rlone (* SIMPLIFIED *)

    method build_RNo (env : 'env) = assert false (* SIMPLIFIED *)

    method build_ROne (env : 'env) = assert false (* SIMPLIFIED *)

    method build_RSome (env : 'env) = assert false (* SIMPLIFIED *)

    (************************** exp  ********************************)

    method build_exp (env : 'env) _ pe' _ _ = pe'


    (* re-defining this method to avoid going down in the block as a
       substitution must be made first *)
    method visit_Compr env _visitors_c0 _visitors_c1 =
      let _visitors_r0 =
        self#visit_list self#visit_sim_binding env _visitors_c0
      in
      let _visitors_r1 = [true_]  in
      self#build_Compr env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    method private allocate_sbs_to_tuples vars l = match vars with
      | [] -> []
      | (v, r)::tl ->
          let xs, ys = List.take_drop Option.(get_exn r.G.arity) l in
          Tuple.of_list1 xs :: self#allocate_sbs_to_tuples tl ys

    (* shape: [{ sb1, sb2,... | b }]. Each [sb] is of shape [disj x1, x2 : e] .

       The first item implies that we have to fold over the [sb]'s to substitute
       previously-bound variables. In the following function, we perform these
       substitutions and then compute separately the semantics of every binding,
       before computing the whole resulting formula.
    *)
    method build_Compr (env : 'env) sbs b _ _ = fun tuple ->
      (* assert (List.for_all (fun (disj, _, _) -> not disj) sbs); *)
      (* compute the subtitution of elements in [tuple] for all bound variables *)
      let ranging_vars = 
        List.(flat_map (fun (_, vs, r) ->
              List.map (fun (Elo.BVar v) -> (v, r)) vs) sbs) in
      let split_tuples =
        self#allocate_sbs_to_tuples ranging_vars @@ Tuple.to_list tuple in
      let split =  List.map Fun.(G.ident % Elo.tuple_ident) split_tuples in
      let all_vars =
        List.flat_map (fun (_, vs, _) ->
              List.map (fun (Elo.BVar v) -> v) vs) sbs in
      let sub = List.combine all_vars split in
      (* Msg.debug (fun m -> *)
      (*       m "build_Compr: tuple = %a split = %a sub = %a " *)
      (*         Tuple.pp tuple *)
      (*         Fmtc.(brackets *)
      (*               @@ list ~sep:sp *)
      (*               @@ Elo.pp_prim_exp) split *)
      (*         Fmtc.(list @@ brackets  *)
      (*               @@ pair ~sep:(const string " <- ") Var.pp *)
      (*               @@ Elo.pp_prim_exp) sub *)
      (*     ); *)
      (* semantics of [b] is [[ b [atoms / variables] ]] *)
      let b' = self#visit_prim_fml env
        @@ Elo.substitute#visit_prim_fml sub
        @@ G.block b
      in
      (* get a tuple out of a list of variables in sub *)
      let sub_tuples = List.combine all_vars split_tuples in
      let get_tuple vs =
        vs
        |> List.flat_map (fun (Elo.BVar v) ->
              Tuple.to_list @@
              List.Assoc.get_exn ~eq:Var.equal v sub_tuples) 
        |> Tuple.of_list1
      in
      (* we bluntly substitute in ranges also, as bound-variable names are
         unique!, instead of folding more finely over the list of bindings *)
      let ranges =
        List.map (fun (_, vs, e) ->
              self#visit_exp env
                (Elo.substitute#visit_exp sub e)
              @@ get_tuple vs) sbs
      in
      conj (b' :: ranges)

    method build_Iden (env : 'env) = fun tuple -> 
      assert (Tuple.arity tuple = 2);
      if Atom.equal (Tuple.ith 0 tuple) (Tuple.ith 1 tuple) then
        true_
      else
        false_

    method build_BoxJoin (env : 'env) call args call' args' = (* SIMPLIFIED *)
      assert false

    method build_Ident (env : 'env) _ id = fun tuple ->
      let open Elo in
      match id with
        | Var v ->
            failwith @@ "Elo_to_LTL1.build_Ident <-- %a, \
                         theoretically unreachable case" ^ Var.to_string v
        | Tuple bs ->
            if Tuple.equal bs tuple then true_ else false_
        | Name r ->
            let { must; may; _ } =
              env#must_may_sup @@
              G.exp (Some (env#arity r)) Location.dummy @@ G.ident id
            in
            (* Msg.debug (fun m -> *)
            (*       m "build_Ident: must/may(%a) = %a / %a | tuple = %a" *)
            (*         Name.pp r *)
            (*         TS.pp must *)
            (*         TS.pp may *)
            (*         Tuple.pp tuple); *)
            if TS.mem tuple must then
              true_
            else if TS.mem tuple may then 
              env#make_atom r tuple
            else
              false_

    method build_None_ (env : 'env) = fun _ -> false_

    method build_Univ (env : 'env) = fun _ -> true_

    method build_Prime (env : 'env) _ e' = fun tuple -> next @@ e' tuple

    method build_RIte (env : 'env) _ _ _ f_r e1_r e2_r = fun tuple -> 
      f_r @=> lazy (e1_r tuple +&& lazy (not_ f_r @=> lazy (e2_r tuple)))


    (* rbinop *)

    method build_RBin (env : 'env) f1 op f2 f1' op' f2' =
      op' f1 f2 f1' f2'

    method build_Union (env : 'env) _ _ e1 e2 =
      (fun x -> e1 x +|| lazy (e2 x))

    method build_Inter (env : 'env) _ _ e1 e2 = fun x -> e1 x +&& lazy (e2 x)

    (* method build_Join (env : 'env) r s r' s' =  fun tuple -> *)
    (*   let sup_r = (env#must_may_sup r).sup in *)
    (*   let sup_s = (env#must_may_sup s).sup in *)
    (*   let s1 = TS.to_seq sup_r in *)
    (*   let s2 = TS.to_seq sup_s in *)
    (*   let[@landmark] eligible_pairs = *)
    (*     Sequence.product s1 s2 *)
    (*     |> Sequence.filter (fun (t1, t2) -> Tuple.is_in_join tuple t1 t2) *)
    (*   in *)
    (*   Msg.debug *)
    (*     (fun m -> m "Elo_to_LTL1.build_Join <-- \ *)
           (*                  %a.%a@\nsup(%a) = %a@\nsup(%a) = %a@\neligible_pairs =@ %a" *)
    (*                 (Elo.pp_exp) r *)
    (*                 (Elo.pp_exp) s *)
    (*                 (Elo.pp_exp) r *)
    (*                 TupleSet.pp sup_r *)
    (*                 (Elo.pp_exp) s *)
    (*                 TupleSet.pp sup_s *)
    (*                 (Sequence.pp_seq *)
    (*                  @@ Fmtc.brackets @@ Fmt.pair ~sep:Fmtc.sp Tuple.pp Tuple.pp) *)
    (*                 eligible_pairs *)
    (*     ); *)
    (*   vee ~range:eligible_pairs (fun (bs, cs) -> lazy (r' bs +&& lazy (s' cs))) *)
    (*     [@landmark "build_Join"] *)


    (* method build_Join (env : 'env) r s r' s' =  fun tuple -> *)
    (*   let open Sequence in *)
    (*   let rseq = (env#must_may_sup r).sup |> TS.to_seq in *)
    (*   let sseq = (env#must_may_sup s).sup |> TS.to_seq in *)
    (*   let[@landmark] eligible_pairs = *)
    (*     fold (fun acc rtuple -> *)
    (*           filter_map *)
    (*             (fun stuple -> *)
    (*                if Tuple.is_in_join tuple rtuple stuple then *)
    (*                  Some (rtuple, stuple) *)
    (*                else *)
    (*                  None) sseq *)
    (*           |> append acc) empty rseq *)
    (*   in *)

    (*   (\* Msg.debug *\) *)
    (*   (\*   (fun m -> m "Elo_to_LTL1.build_Join <-- \ *\) *)
         (*             (\*                %a.%a@\nsup(%a) = %a@\nsup(%a) = %a@\neligible_pairs =@ %a" *\) *)
    (*   (\*               (Elo.pp_exp) r *\) *)
    (*   (\*               (Elo.pp_exp) s *\) *)
    (*   (\*               (Elo.pp_exp) r *\) *)
    (*   (\*               TupleSet.pp (env#sup r) *\) *)
    (*   (\*               (Elo.pp_exp) s *\) *)
    (*   (\*               TupleSet.pp (env#sup s) *\) *)
    (*   (\*               (Sequence.pp_seq  *\) *)
    (*   (\*                @@ Fmtc.brackets @@ Fmt.pair ~sep:Fmtc.sp Tuple.pp Tuple.pp) *\) *)
    (*   (\*               eligible_pairs *\) *)
    (*   (\*   ); *\) *)
    (*   vee ~range:eligible_pairs (fun (bs, cs) -> lazy (r' bs +&& lazy (s' cs))) *)
    (*     [@landmark "build_Join"] *)




    method build_Join (env : 'env) r s r' s' =  fun tuple ->
      let open List in
      (* Msg.debug (fun m -> *)
      (*       m "build_Join <-- [[%a . %a]](%a) " *)
      (*         Elo.pp_exp r *)
      (*         Elo.pp_exp s *)
      (*         Tuple.pp tuple); *)
      let rlist = lazy (TS.to_list (env#must_may_sup r).sup) in
      let slist = lazy (TS.to_list (env#must_may_sup s).sup) in
      let pairs = eligible_pairs (tuple, rlist, slist) in
      (* Msg.debug (fun m -> *)
      (*       m "build_Join: eligible pairs: %a" *)
      (*         (Sequence.pp_seq ~sep:"," *)
      (*          @@ Fmtc.parens @@ *)
      (*          Fmtc.(pair ~sep:comma) Tuple.pp Tuple.pp) *)
      (*         pairs *)
      (*     ); *)
      vee ~range:pairs (fun (bs, cs) ->
            lazy (r' bs +&& lazy (s' cs)))
      (* |> Fun.tap (fun res -> *)
      (*       Msg.debug (fun m -> *)
      (*             m "build_Join [[%a . %a]](%a) --> %a" *)
      (*               Elo.pp_exp r *)
      (*               Elo.pp_exp s *)
      (*               Tuple.pp tuple *)
      (*               Ltl.pp res *)
      (*           )) *)



    method build_LProj (env : 'env) _ _ s' r' = fun tuple -> 
      r' tuple +&& lazy (s' @@ Tuple.(of_list1 [ith 0 tuple]))

    method build_Prod (env : 'env) r s r' s' = fun tuple ->
      let ar_r = Option.get_exn r.G.arity in
      let t1, t2 = Tuple.split tuple ar_r in
      r' t1 +&& lazy (s' t2)
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug *)
    (*         (fun m -> m "build_Prod [[%a->%a]](%a) (split as %a, %a) = %a (ar(%a) = %d)" *)
    (*                     Elo.pp_exp r *)
    (*                     Elo.pp_exp s *)
    (*                     Tuple.pp tuple  *)
    (*                     Tuple.pp t1  *)
    (*                     Tuple.pp t2 *)
    (*                     Ltl.pp res *)
    (*                     Elo.pp_exp r *)
    (*                     ar_r *)
    (*         )) *)


    method build_RProj (env : 'env) _ _ r' s' = fun tuple -> 
      let lg = Tuple.arity tuple in
      r' tuple +&& lazy (s' @@ Tuple.of_list1 [Tuple.ith (lg - 1) tuple])

    method build_Diff (env : 'env) _ _ e' f' = fun x -> e' x +&& lazy (not_ (f' x))

    method build_Over (env : 'env) _ _ r' s' = fun tuple -> 
      s' tuple +|| lazy (r' tuple +&& lazy (not_ @@ s' Tuple.(of_list1 [ith 0 tuple])))


    (* runop *)

    method build_RUn (env : 'env) _ e op e_r = op e e_r

    method build_RTClos (env : 'env) r _ = fun tuple ->
      self#build_Iden env tuple +|| lazy (self#visit_RUn env G.TClos r tuple)

    method build_Transpose (env : 'env) _ r' = fun tuple -> 
      r' @@ Tuple.transpose tuple

    method build_TClos (env : 'env) r r' =
      (* Msg.debug *)
      (*   (fun m -> m "Elo_to_LTL1.build_TClos <-- %a" *)
      (*               Elo.pp_exp r) *)
      (* ; *)
      let { sup ; _ } = env#must_may_sup r in
      let[@landmark] k = compute_tc_length sup in
      (* let tc_naif = iter_tc r k in *)
      let[@landmark] tc_square = iter_squares r k in
      (* let suptc =  *)
      (*   (env#must_may_sup (G.exp Location.dummy @@ G.runary G.tclos r)).sup *)
      (* in *)
      (* let suptc2 = *)
      (*   (env#must_may_sup tc_square).sup *)
      (* in *)
      Msg.debug (fun m ->
            m "borne de TC: (%d)" k);
      (* Msg.debug (fun m -> *)
      (*     m "terme de TC naif : (%a)" (Elo.pp_exp) (tc_naif)); *)
      Msg.debug (fun m ->
            m "terme de TC avec carrés itératifs : (%a)" (Elo.pp_exp) (tc_square));
      (* Msg.debug (fun m -> *)
      (*     m "sup(%a) = %a" (Elo.pp_prim_exp) (G.runary G.tclos r) *)
      (*       TS.pp suptc); *)
      (* Msg.debug (fun m -> *)
      (*     m "sup(%a) = %a" (Elo.pp_exp) (tc_square) *)
      (*       TS.pp suptc2); *)

      self#visit_exp env tc_square [@landmark "build_TClos/visit_exp"]




    (*********************************** iexp **************************************)

    method build_iexp (env : 'env) _ iexp' _ = iexp'

    method build_IBin (env : 'env) _ _ _ i1' op' i2' = op' i1' i2'

    method build_IUn (env : 'env) _ _ op' i' = op' i'

    method build_Num (env : 'env) _ = num

    method build_Add (env : 'env) = plus

    method build_Neg (env : 'env) = neg

    method build_Sub (env : 'env) = minus

    method build_Card (env : 'env) r r' =
      let { must; may; _ } = env#must_may_sup r in
      let must_card = num @@ TS.size must in
      let may_card =
        count @@ List.map r' @@ TS.to_list may
      in
      plus must_card may_card


  end                           (* end converter *)

  (* set of atoms, to gather rigid and flexiblae variables *)
  module AS =
    Set.Make(struct
      type t = Ltl.atomic
      let compare = Ltl.compare_atomic
    end)

  class environment (elo : Elo.t) = object (self : 'self)
    val mutable flexible_atoms = AS.empty

    val mutable rigid_atoms = AS.empty

    method arity name =
      match Domain.get name elo.Elo.domain with
        | None -> assert false
        | Some rel -> Relation.arity rel

    val cached_bounds_exp =
      (* cancel caching as long as hashconsing is not implemented *)
      (* CCCache.(with_cache (unbounded ~eq:Elo.equal_prim_exp 2973) *)
      (*          @@ bounds_prim_exp elo.domain) *)
      bounds_prim_exp elo.Elo.domain

    method must_may_sup (e : (Elo.var, Elo.ident) G.exp) =
      cached_bounds_exp e.G.prim_exp
        [@landmark "must_may_sup"]

    method make_atom (name : Name.t) (t : Tuple.t) =
      assert (Domain.mem name elo.Elo.domain);
      let atom = make_atomic name t in
      (if Domain.get_exn name elo.Elo.domain |> Relation.is_const then
         rigid_atoms <- AS.add atom rigid_atoms
       else
         flexible_atoms <- AS.add atom flexible_atoms);
      Ltl.atomic atom

    method atoms =              (* TODO remove seq and return set *)
      Pair.map_same AS.to_seq (rigid_atoms, flexible_atoms)

    method is_const (name : Name.t) =
      assert (Domain.mem name elo.Elo.domain);
      Domain.get_exn name elo.Elo.domain |> Relation.is_const
  end

  (* Computes the color (Invar, Static_prop, Init or Temporal) of an
  elo formula *)                                     
  let color elo elo_fml =
    let open Elo in
    let open Invar_computation in
    let env = new environment elo in
    let color = (new invarComputation)#visit_fml env elo_fml in
    color
      
  (* Converts an Elo formula to an LTL formula, gathering at the same time the
     rigid and flexible variables having appeared during the walk. *)
  let convert elo elo_fml =
    let open Elo in
    let env = new environment elo in    
    let ltl_fml = (new converter)#visit_fml env elo_fml in
    let (rigid, flexible) = env#atoms in
    (rigid, flexible, ltl_fml)

      
end
