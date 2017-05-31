open Containers

module G = GenGoal
module TS = TupleSet

module MakeLtlConverter (Ltl : LTL.S) = struct
  open Ltl
  open Ltl.Infix
  open TS.Infix

  type goal = Elo.goal

  
  let substs_of_sim_binding ~disj (vars : Elo.var list) (dom : Tuple.t list) =
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
    (* Cast. This ain't cool as we lose all the benefit of the lazy nature of sequences *)
    |> to_seq
    
  class ['env] converter = object (self : 'self)
    inherit ['self] GenGoalRecursor.recursor as super

    method visit_'v (env : 'env) = Fun.id

    method visit_'i (env : 'env) = Fun.id

    (* fml  *)                        

    method build_fml (env : 'env) _ ltl _ = ltl

    method build_Sat (env : 'env) _ = conj

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
        (fun m -> m "Elo_to_LTL1.build_Quant <-- %a"
                    (G.pp_prim_fml Elo.pp_var Elo.pp_ident)
                    (G.quant quant sim_bindings blk)
        );
      match quant with
        | G.Lone | G.One ->
            failwith "Elo_to_LTL1.build_Quant: lone/one not implemented yet"
        | G.All | G.Some_ | G.No ->
            assert (List.length sim_bindings = 1); (* SIMPLIFIED *)
            let disj, xs, s = List.hd sim_bindings in
            let _, _, s' = List.hd sim_bindings' in
            let sub_for tuples =
              let tuples_as_idents =
                List.map (fun t1 -> G.ident @@ Elo.Tuple t1) tuples in
              let xs_as_vars = List.map (fun (Elo.BVar v) -> v) xs in
              (* we zip the bound variables and the 1-tuples to get a list of
                 substitutions *)
              List.combine xs_as_vars tuples_as_idents
            in

            (* [pos_or_neg] tells whether the quantifier was a [no ...], in
               which case we consider the whole as [all ... | not ...]. [neutral]
               is the neutral element for the wedge or vee. *)
            let (bigop, smallop, pos_or_neg, neutral) = match quant with
              | G.All -> (wedge, (+&&), Fun.id, G.true_)
              | G.Some_ -> (vee, (+||), Fun.id, G.false_)
              | G.No -> (wedge, (+&&), not_, G.true_)
              | G.Lone | G.One -> assert false
            in
            let mustpart =
              bigop
                ~range:(substs_of_sim_binding ~disj xs @@ TS.to_list @@ env#must s)
                (fun tuples -> 
                   pos_or_neg
                   @@ self#visit_prim_fml env (* [[...]] *)
                   @@ Elo.substitute#visit_prim_fml (sub_for tuples)
                   @@ G.block blk) (* b [as / xs] *)
            in
            let maypart =
              bigop
                ~range:(substs_of_sim_binding ~disj xs @@ TS.to_list @@ env#may s)
                (fun tuples ->
                   (* concat because semantics of expressions expects *one* tuple *)
                   s' (List.fold_left Tuple.(fun acc t -> (@@@) (acc, t))
                         (List.hd tuples) (List.tl tuples))
                   @=>
                   pos_or_neg
                   @@ self#visit_prim_fml env (* [[...]] *)
                   @@ Elo.substitute#visit_prim_fml (sub_for tuples)
                   @@ G.block blk) (* b [as / xs] *)
            in
            smallop mustpart maypart 

    method build_One (env : 'env) = G.One

    method build_Lone (env : 'env) = G.Lone

    method build_All (env : 'env) = G.All

    method build_No (env : 'env) = G.No 

    method build_Some_ (env : 'env) = G.Some_    

    (* lbinop *)      

    method build_LBin (env : 'env) f1 _ f2 f1_r op f2_r = op f1 f2 f1_r f2_r

    method build_And (env : 'env) _ _ = and_          

    method build_Iff (env : 'env) _ _ = iff

    method build_Imp (env : 'env) _ _ = implies

    method build_U (env : 'env) _ _ = until

    method build_Or (env : 'env) _ _ = or_

    method build_R (env : 'env) _ _ = releases

    method build_S (env : 'env) _ _ = since

    (* lunop *)                     

    method build_LUn (env : 'env) _ f op f' = op f f'

    method build_X (env : 'env) _ = next

    method build_F (env : 'env) _ = eventually

    method build_G (env : 'env) _ = always

    method build_H (env : 'env) _ = historically

    method build_O (env : 'env) _ = once

    method build_P (env : 'env) _ = yesterday

    method build_Not (env : 'env) _ = not_

    (* compo_op *)

    method build_RComp (env : 'env) f1 _ f2 f1' op' f2' = op' f1 f2 f1' f2'

    method build_REq (env : 'env) r s r' s' =
      self#build_In env r s r' s' +&& self#build_In env s r s' r'

    method build_In (env : 'env) r s r' s' =
      let must_r = env#must r in
      let may_r = env#may r in
      wedge ~range:(TS.to_seq must_r) s'
      +&& wedge ~range:(TS.to_seq may_r) (fun bs -> r' bs @=> s' bs)

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

    method build_exp (env : 'env) _ exp' _ _ _ _ _ = exp'


    (* re-defining this method to avoid going down in the block as a
       substitution must be made first *)
    method visit_Compr env _visitors_c0 _visitors_c1 =
      let _visitors_r0 =
        self#visit_list self#visit_sim_binding env _visitors_c0
      in
      let _visitors_r1 = [true_]  in
      self#build_Compr env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    (* shape: [{ sb1, sb2,... | b }]. Each [sb] is of shape [x1, x2 : e] (the
       [disj]'s have already been simplified).

       The first item implies that we have to fold over the [sb]'s to substitute
       previously-bound variables. In the following function, we perform these
       substitutions and then compute separately the semantics of every binding,
       before computing the whole resulting formula.
    *)
    method build_Compr (env : 'env) sbs b _ _ = fun tuple ->
      assert (List.for_all (fun (disj, _, _) -> not disj) sbs);
      (* compute the subtitution of elements in [tuple] for all bound variables *)
      let split_tuples = Tuple.to_1tuples tuple in
      let split =  List.map Fun.(G.ident % Elo.tuple_ident) split_tuples in
      let all_vars =
        List.flat_map (fun (_, vs, _) ->
              List.map (fun (Elo.BVar v) -> v) vs) sbs in
      let sub = List.combine all_vars split in
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
            if tuple $: Domain.must r env#domain then
              true_
            else if tuple $: Domain.may r env#domain then
              atom r tuple
            else
              false_

    method build_None_ (env : 'env) = fun _ -> false_

    method build_Univ (env : 'env) = fun _ -> true_

    method build_Prime (env : 'env) _ e' = fun tuple -> next @@ e' tuple

    method build_RIte (env : 'env) _ _ _ f_r e1_r e2_r = fun tuple -> 
      f_r @=> e1_r tuple +&& not_ f_r @=> e2_r tuple


    (* rbinop *)

    method build_RBin (env : 'env) f1 _ f2 f1' op' f2' = op' f1 f2 f1' f2'

    method build_Union (env : 'env) _ _ e1 e2 = fun x -> e1 x +|| e2 x

    method build_Inter (env : 'env) _ _ e1 e2 = fun x -> e1 x +&& e2 x

    method build_Join (env : 'env) r s r' s' =  fun tuple ->
      let eligible_pairs =
        let s1 = TS.to_seq @@ env#sup r in
        let s2 = TS.to_seq @@ env#sup s in
        Sequence.product s1 s2
        |> Sequence.filter (fun (t1, t2) -> Tuple.is_in_join tuple t1 t2)
      in
      (* Msg.debug *)
      (*   (fun m -> m "Elo_to_LTL1.build_Join <-- \ *)
      (*                %a.%a@\nsup(%a) = %a@\nsup(%a) = %a@\neligible_pairs =@ %a" *)
      (*               (G.pp_exp Elo.pp_var Elo.pp_ident) r *)
      (*               (G.pp_exp Elo.pp_var Elo.pp_ident) s *)
      (*               (G.pp_exp Elo.pp_var Elo.pp_ident) r *)
      (*               TupleSet.pp (env#sup r) *)
      (*               (G.pp_exp Elo.pp_var Elo.pp_ident) s *)
      (*               TupleSet.pp (env#sup s) *)
      (*               (Sequence.pp_seq  *)
      (*                @@ Fmtc.brackets @@ Fmt.pair ~sep:Fmtc.sp Tuple.pp Tuple.pp) *)
      (*               eligible_pairs *)
      (*   ); *)
      vee ~range:eligible_pairs (fun (bs, cs) -> r' bs +&& s' cs)


    method build_LProj (env : 'env) _ _ s' r' = fun tuple -> 
      r' tuple +&& (s' @@ Tuple.(of_list1 [ith 0 tuple]))

    method build_Prod (env : 'env) r s r' s' = fun tuple ->
      let ar_r = TS.inferred_arity @@ env#sup r in
      let t1, t2 = Tuple.split tuple ar_r in
      r' t1 +&& s' t2


    method build_RProj (env : 'env) _ _ r' s' = fun tuple -> 
      let lg = Tuple.arity tuple in
      r' tuple +&& (s' @@ Tuple.of_list1 [Tuple.ith (lg - 1) tuple])

    method build_Diff (env : 'env) _ _ e' f' = fun x -> e' x +&& not_ (f' x)

    method build_Over (env : 'env) _ _ r' s' = fun tuple -> 
      s' tuple +|| (r' tuple +&& (not_ @@ s' Tuple.(of_list1 [ith 0 tuple])))


    (* runop *)

    method build_RUn (env : 'env) _ e op e_r = op e e_r

    method build_RTClos (env : 'env) r r' = fun tuple ->
      self#build_Iden env tuple +|| self#visit_RUn env G.TClos r tuple

    method build_Transpose (env : 'env) _ r' = fun tuple -> 
      r' @@ Tuple.transpose tuple

    method build_TClos (env : 'env) r r' = failwith "build_TClos BRUNEL !!!"

    (*********************************** iexp **************************************)

    method build_iexp (env : 'env) _ iexp' _ = iexp'

    method build_IBin (env : 'env) _ _ _ i1' op' i2' = op' i1' i2'

    method build_IUn (env : 'env) _ _ op' i' = op' i'

    method build_Num (env : 'env) _ = num

    method build_Add (env : 'env) = plus

    method build_Neg (env : 'env) = neg

    method build_Sub (env : 'env) = minus

    method build_Card (env : 'env) r r' =
      let must_card = num @@ TS.size @@ env#must r in
      let may_card =
        count @@ List.map r' @@ TS.to_list @@ env#may r
      in
      plus must_card may_card


  end

  class environment (elo : Elo.t) = object (self : 'self)
    method domain = Elo.(elo.domain)
    method must (e : (Elo.var, Elo.ident) G.exp) = Lazy.force e.G.must
    method may (e : (Elo.var, Elo.ident) G.exp) = Lazy.force e.G.may
    method sup (e : (Elo.var, Elo.ident) G.exp) = Lazy.force e.G.sup
  end

  
  let convert elo =
    let open Elo in
    let env = new environment elo in
    let G.Sat fmls = elo.goal in
    List.map ((new converter)#visit_fml env) fmls
  
end
