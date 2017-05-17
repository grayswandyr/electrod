open Containers


module MakeLtlConverter (Ltl : LTL.S) = struct
  open Ltl
  open Ltl.Infix
  open TupleSet.Infix

  type goal = Elo.goal

    
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

    (* lo_quant *)

    method build_QLO (env : 'env) quant bindings block quant' bindings' block' =
      match quant with
        | GenGoal.One
        | GenGoal.Lone -> failwith "build_QLO"

    method build_One (env : 'env) = GenGoal.One

    method build_Lone (env : 'env) = GenGoal.Lone

    (* ae_quant *)

    method build_QAEN (env : 'env) quant sim_bindings blk _ sim_bindings' _ = 
      assert (List.length sim_bindings = 1); (* SIMPLIFIED *)
      let disj, xs, s = List.hd sim_bindings in
      let _, _, s' = List.hd sim_bindings' in
      (* filter out "diagonal" tuples of a tuple set if [disj = true] *)
      let filter_out ts =
        if disj then TupleSet.filter Tuple.all_different ts
        else ts
      in
      let must_s, may_s = (filter_out @@ env#must s, filter_out @@ env#may s) 
      in
      (* The range quantified upon (by wedge or vee) will be made of tuples of
         the arity of [s^(nb of bound variables)]. [split] is the result of
         breaking these tuples into 1-tuples, one for each bound variable.  *)
      let sub_for tuple =    
        let split = Tuple.to_1tuples tuple
                    |> List.map (fun t1 -> GenGoal.ident @@ Elo.Tuple t1) in
        let xs_as_vars = List.map (fun (Elo.BVar v) -> v) xs in
        (* we zip the bound variables and the 1-tuples to get a list of substitutions  *)
        List.combine xs_as_vars split
        |> Elo.substitute#visit_prim_fml (* and we use them to substitute *)
      in
      (* [positive_or_negative] tells whether the quantifier was a [no ...], in
         which case we consider the whole as [all ... | not ...] *)
      let (bigop, smallop, positive_or_negative) = match quant with
        | GenGoal.All -> (wedge, (+&&), Fun.id)
        | GenGoal.Some_ -> (vee, (+||), Fun.id)
        | GenGoal.No -> (wedge, (+&&), not_)
      in
      let mustpart =
        bigop ~range:(TupleSet.to_seq must_s)
          (fun tuple ->
             positive_or_negative
             @@ self#visit_prim_fml env (* [[...]] *)
             @@ (GenGoal.block blk |> sub_for tuple)) (* b [as / xs] *)
      in
      let maypart =
        bigop ~range:(TupleSet.to_seq may_s)
          (fun tuple ->
             s' tuple 
             @=>
             positive_or_negative
             @@ self#visit_prim_fml env (* [[...]] *)
             @@ (GenGoal.block blk |> sub_for tuple)) (* b [as / xs] *)
      in
      smallop mustpart maypart 

    method build_All (env : 'env) = GenGoal.All

    method build_No (env : 'env) = GenGoal.No 

    method build_Some_ (env : 'env) = GenGoal.Some_    

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
      wedge ~range:(TupleSet.to_seq must_r) s'
      +&& wedge ~range:(TupleSet.to_seq may_r) (fun bs -> r' bs @=> s' bs)

    method build_NotIn (env : 'env) r s r' s' = not_ @@ self#build_In env r s r' s'

    method build_RNEq (env : 'env) r s r' s' = not_ @@ self#build_REq env r s r' s'

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

    method build_RLone (env : 'env) = GenGoal.rlone (* SIMPLIFIED *)

    method build_RNo (env : 'env) = assert false (* SIMPLIFIED *)

    method build_ROne (env : 'env) = assert false (* SIMPLIFIED *)

    method build_RSome (env : 'env) = assert false (* SIMPLIFIED *)

    (************************** exp  ********************************)

    method build_exp (env : 'env) _ exp' _ _ _ _ _ = exp'

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
      let split =  List.map Fun.(GenGoal.ident % Elo.tuple_ident) split_tuples in
      let all_vars =
        List.flat_map (fun (_, vs, _) ->
              List.map (fun (Elo.BVar v) -> v) vs) sbs in
      let sub = List.combine all_vars split in
      (* semantics of [b] is [[ b [atoms / variables] ]] *)
      let b' = self#visit_prim_fml env
        @@ Elo.substitute#visit_prim_fml sub
        @@ GenGoal.block b
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
        | Var _ -> assert false (* shoud've been substituted from above *)
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
      let sup_r = env#sup r in
      let sup_s = env#sup s in
      let eligible_pairs =
        let s1 = TupleSet.to_seq sup_r in
        let s2 = TupleSet.to_seq sup_s in
        Sequence.product s1 s2
        |> Sequence.filter (fun (t1, t2) -> Tuple.is_in_join tuple t1 t2)
      in
      vee ~range:eligible_pairs (fun (bs, cs) -> r' bs +&& s' cs)


    method build_LProj (env : 'env) _ _ s' r' = fun tuple -> 
      r' tuple +&& (s' @@ Tuple.(of_list1 [ith 0 tuple]))

    method build_Prod (env : 'env) r s r' s' = fun tuple ->
      let ar_r = TupleSet.inferred_arity @@ env#sup r in
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
      self#build_Iden env tuple +|| self#visit_RUn env GenGoal.TClos r tuple

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
      let must_card = num @@ TupleSet.size @@ env#may r in
      let may_card =
        count @@ List.map r' @@ TupleSet.to_list @@ env#may r
      in
      plus must_card may_card


  end

  class environment (elo : Elo.t) = object (self : 'self)
    method domain = Elo.(elo.domain)
    method must (e : (Elo.var, Elo.ident) GenGoal.exp) = e.must
    method may (e : (Elo.var, Elo.ident) GenGoal.exp) = e.may
    method sup (e : (Elo.var, Elo.ident) GenGoal.exp) = e.sup
  end

  
  let convert elo =
    let open Elo in
    let env = new environment elo in
    (new converter)#visit_t env elo.goal
  
end
