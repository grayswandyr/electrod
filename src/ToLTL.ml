open Containers


module MakeLtlConverter (Ltl : LTL.S) = struct
  open Ltl
  open Ltl.Infix
  open TupleSet.Infix


  
  class ['self] convert = object (self : 'self)
    inherit [_] GenGoalRecursor.recursor as super
      
    method visit_'v env = Fun.id

    method visit_'i env = Fun.id

    (* fml  *)                        

    method build_fml env _ ltl _ = ltl

    method build_Sat env _ = conj

    method build_True env = true_
      
    method build_False env = false_

    method build_Block env _ = conj

    method build_FIte env _ _ _ = ifthenelse 

    method build_Let env bindings block bindings' block'= assert false (* SIMPLIFIED *)

    (* lo_quant *)

    method build_QLO env quant bindings block quant' bindings' block' = match quant with
      | GenGoal.One
      | GenGoal.Lone -> failwith "build_QLO"

    method build_One env = GenGoal.One
                             
    method build_Lone env = GenGoal.Lone

    (* ae_quant *)

    method build_QAEN env quant sim_bindings blk _ _ _ =
      assert (List.length sim_bindings = 1); (* SIMPLIFIED *)
      let disj, xs, s = List.hd sim_bindings in
      let must_s, may_s = Elo.must env.Elo.domain s, Elo.may env.Elo.domain s in
      let s' = self#visit_exp env s in
      let lg = List.length xs in
      let range ts =
        let open Sequence in
        repeat ts
        |> take (lg - 1)        (* -1 because we use the remaining one as the seed for the fold below *)
        |> fold TupleSet.product ts
        |> TupleSet.filter Tuple.all_different
        |> TupleSet.to_seq
      in
      let sub_for tuple =       (* substitution mapping for a specific tuple *)
        let split = Tuple.to_1tuples tuple |> List.map (fun t1 -> GenGoal.ident @@ Elo.Tuple t1) in
        let xs_as_vars = List.map (fun (Elo.BVar v) -> v) xs in
        Elo.substitute#visit_prim_fml (List.combine xs_as_vars split)
      in
      let (bigop, smallop, ok_or_not) = match quant with
        | GenGoal.All -> (wedge, (+&&), Fun.id)
        | GenGoal.Some_ -> (vee, (+||), Fun.id)
        | GenGoal.No -> (wedge, (+&&), not_)
      in
      let mustpart =
        bigop ~range:(range must_s)
          (fun tuple -> ok_or_not @@ self#visit_prim_fml env @@ sub_for tuple @@ GenGoal.block blk)
      in
      let maypart =
        bigop ~range:(range may_s)
          (fun tuple ->
             s' tuple 
             @=> ok_or_not @@ self#visit_prim_fml env @@ sub_for tuple @@ GenGoal.block blk)
      in
      smallop mustpart maypart
                        
    method build_All env = GenGoal.All

    method build_No env = GenGoal.No 

    method build_Some_ env = GenGoal.Some_    

    (* lbinop *)      

    method build_LBin env f1 _ f2 f1_r op f2_r = op f1 f2 f1_r f2_r
    
    method build_And env _ _ = and_          

    method build_Iff env _ _ = iff

    method build_Imp env _ _ = implies

    method build_U env _ _ = until

    method build_Or env _ _ = or_

    method build_R env _ _ = releases

    method build_S env _ _ = since

    (* lunop *)                     
      
    method build_LUn env _ f op f' = op f f'

    method build_X env _ = next

    method build_F env _ = eventually

    method build_G env _ = always

    method build_H env _ = historically

    method build_O env _ = once

    method build_P env _ = yesterday

    method build_Not env _ = not_

    (* compo_op *)

    method build_RComp env f1 _ f2 f1' op' f2' = op' f1 f2 f1' f2'

    method build_REq env r s r' s' =
      self#build_In env r s r' s' +&& self#build_In env s r s' r'

    method build_In env r s r' s' =
      let must_r = Elo.must env.Elo.domain r in
      let may_r = Elo.may env.Elo.domain r in
      wedge ~range:(TupleSet.to_seq must_r) s'
      +&& vee ~range:(TupleSet.to_seq may_r) (fun bs -> r' bs @=> s' bs)

    method build_NotIn env r s r' s' = not_ @@ self#build_In env r s r' s'

    method build_RNEq env r s r' s' = not_ @@ self#build_REq env r s r' s'

    (* icomp_op *)

    method build_IComp env e1 _ e2 e1_r op e2_r = op e1 e2 e1_r e2_r

    method build_Gt env = failwith "build_Gt"

    method build_Gte env = failwith "build_Gte"

    method build_IEq env = failwith "build_IEq"

    method build_INEq env = failwith "build_INEq"

    method build_Lt env = failwith "build_Lt"

    method build_Lte env = failwith "build_Lte"

    (* rqualify *)

    method build_Qual env _ r q' r' = assert false (* SIMPLIFIED *)

    method build_RLone env = assert false

    method build_RNo env = assert false

    method build_ROne env = assert false

    method build_RSome env = assert false

    (************************** exp  ********************************)

    method build_exp env _ exp _ = exp

    method build_Compr env sbs b sbs' b' = failwith "build_Compr"

    method build_Iden env = fun tuple -> 
      assert (Tuple.arity tuple = 2);
      if Atom.equal (Tuple.ith 0 tuple) (Tuple.ith 1 tuple) then
        true_
      else
        false_
      
    method build_BoxJoin env call args call' args' = assert false (* SIMPLIFIED *)

    method build_Ident env _ id = fun tuple ->
      let open Elo in
      match id with
        | Var _ -> assert false (* shoud've been substituted from above *)
        | Tuple bs ->
            if Tuple.equal bs tuple then true_ else false_
        | Name r ->
            if tuple $: Domain.must r env.domain then
              true_
            else if tuple $: Domain.may r env.domain then
              atom r tuple
            else
              false_

    method build_None_ env = fun _ -> false_
      
    method build_Univ env = fun _ -> true_

    method build_Prime env e e' = failwith "build_Prime"

    method build_RIte env _ _ _ f_r e1_r e2_r = fun tuple -> 
      f_r @=> e1_r tuple +&& not_ f_r @=> e2_r tuple


    (* rbinop *)

    method build_RBin env f1 _ f2 f1' op' f2' = op' f1 f2 f1' f2'
    
    method build_Union env _ _ e1 e2 = fun x -> e1 x +|| e2 x
                                                           
    method build_Inter env _ _ e1 e2 = fun x -> e1 x +&& e2 x

    method build_Join env r s r' s' =  fun tuple ->
      let sup_r = Elo.sup env.Elo.domain r in
      let sup_s = Elo.sup env.Elo.domain s in
      let eligible_pairs =
        let s1 = TupleSet.to_seq sup_r in
        let s2 = TupleSet.to_seq sup_s in
        Sequence.product s1 s2
        |> Sequence.filter (fun (t1, t2) -> Tuple.is_in_join tuple t1 t2)
      in
      vee ~range:eligible_pairs (fun (bs, cs) -> r' bs +&& s' cs)
        

    method build_LProj env _ _ s' r' = fun tuple -> 
      r' tuple +&& (s' @@ Tuple.(of_list1 [ith 0 tuple]))
    
    method build_Prod env r s r' s' = fun tuple ->
      let ar_r = TupleSet.inferred_arity @@ Elo.sup env.Elo.domain r in
      let t1, t2 = Tuple.split tuple ar_r in
      r' t1 +&& s' t2
      

    method build_RProj env _ _ r' s' = fun tuple -> 
      let lg = Tuple.arity tuple in
      r' tuple +&& (s' @@ Tuple.of_list1 [Tuple.ith (lg - 1) tuple])

    method build_Diff env _ _ e' f' = fun x -> e' x +&& not_ (f' x)

    method build_Over env _ _ r' s' = fun tuple -> 
      s' tuple +|| (r' tuple +&& (not_ @@ s' Tuple.(of_list1 [ith 0 tuple])))


    (* runop *)

    method build_RUn env _ e op e_r = op e e_r

    method build_RTClos env r r' = fun tuple ->
      self#build_Iden env tuple +|| self#visit_RUn env GenGoal.TClos r tuple

    method build_Transpose env _ r' = fun tuple -> 
      r' @@ Tuple.transpose tuple

    method build_TClos env r r' = failwith "build_TClos BRUNEL !!!"
    
    (*********************************** iexp *****************************************)

    method build_iexp env iexp _ = failwith "build_iexp" 

    method build_Num env = failwith "build_Num"

    method build_Add env = failwith "build_Add"

    method build_Neg env = failwith "build_Neg"

    method build_Sub env = failwith "build_Sub"

    method build_Card env = failwith "build_Card"

    method build_IBin env = failwith "build_IBin"

    method build_IUn env = failwith "build_IUn"

  end

  let convert elo goal = (new convert)#visit_t elo goal
  
end
