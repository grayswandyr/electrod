open Containers


module MakeLtlConverter (Ltl : LTL.S) = struct
  open Ltl
  open Ltl.Infix
  open TupleSet.Infix

  
  class ['self] convert = object (self : 'self)
    inherit [_] GenGoal.fold as super

    val mutable r : _ GenGoal.exp option = None
    val mutable s : _ GenGoal.exp option = None
    

    method visit_'v env = Fun.id

    method visit_'i env = Fun.id

    (* fml  *)
                        

    method build_fml env ltl _ = ltl    

    method build_All env = GenGoal.All

    method build_And env = and_

    method build_Block env = conj

    method build_F env = eventually

    method build_FIte env = ifthenelse 

    method build_False env = false_

    method build_G env = always

    method build_Gt env = failwith "build_Gt"

    method build_Gte env = failwith "build_Gte"

    method build_H env = historically

    method build_IComp env e1_r op e2_r = op e1_r e2_r

    method build_IEq env = failwith "build_IEq"

    method build_INEq env = failwith "build_INEq"
          

    method build_Iff env = iff

    method build_Imp env = implies
      

    method build_In env r' s' =
      assert (Option.is_some r && Option.is_some s);
      let must_r = Elo.must env.Elo.domain @@ Option.get_exn r in
      let may_r = Elo.may env.Elo.domain @@ Option.get_exn r in
      wedge ~range:(TupleSet.to_seq must_r) s'
      +&& vee ~range:(TupleSet.to_seq may_r) (fun bs -> r' bs @=> s' bs)
      
                            
                     
      

    method build_LBin env f1_r op f2_r = op f1_r f2_r

    method build_LUn env op f' = op f'

    method build_Let env = failwith "build_Let"

    method build_Lone env = GenGoal.Lone

    method build_Lt env = failwith "build_Lt"

    method build_Lte env = failwith "build_Lte"

    method build_No env = GenGoal.No 

    method build_Not env = not_

    method build_NotIn env r' s' = not_ @@ self#build_In env r' s'

    method build_O env = once

    method build_One env = GenGoal.One

    method build_Or env = or_

    method build_P env = yesterday

    method build_QAEN env quant sim_bindings' block' = match quant with
      | GenGoal.All
      | GenGoal.Some_
      | GenGoal.No -> failwith "build_QAEN"

    method build_QLO env quant bindings' block' = match quant with
      | GenGoal.One
      | GenGoal.Lone -> failwith "build_QLO"

    method build_Qual env = assert false

    method build_R env = releases

    method build_RBin env f1 op f2 = op f1 f2

    method build_RComp env f1 op f2 = op f1 f2

    method build_REq env = failwith "build_REq" 

    method build_RLone env = assert false

    method build_RNEq env r' s' = not_ @@ self#build_REq env r' s'

    method build_S env = since

    method build_Sat env = conj

    method build_Some_ env = GenGoal.Some_

    method build_True env = true_

    method build_U env = until

    method build_Union env e1 e2 = fun x -> e1 x +|| e2 x

    method build_X env = next

    (* exp  *)

    method build_exp env exp _ = exp


    method visit_RComp env left op right =
      r <- Some left;
      s <- Some right;
      super#visit_RComp env left op right
      |> Fun.tap (fun _ -> r <- None; s <- None)
    

    method build_BoxJoin env = assert false (* simplified *)

    method build_Compr env = failwith "build_Compr"

    method build_Diff env = failwith "build_Diff"

    method build_Iden env tuple =
      assert (Tuple.arity tuple = 2);
      if Atom.equal (Tuple.ith 0 tuple) (Tuple.ith 1 tuple) then
        true_
      else
        false_
      

    method build_Ident env id = fun tuple ->
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

    method build_Inter env e1 e2 = fun x -> e1 x +&& e2 x

    method build_Join env r' s' tuple =
      assert (Option.is_some r && Option.is_some s);
      let sup_r = Elo.sup env.Elo.domain @@ Option.get_exn r in
      let sup_s = Elo.sup env.Elo.domain @@ Option.get_exn s in
      let eligible_pairs =
        let s1 = TupleSet.to_seq sup_r in
        let s2 = TupleSet.to_seq sup_s in
        Sequence.product s1 s2
        |> Sequence.filter (Fun.uncurry @@ Tuple.is_in_join tuple)
      in
      vee ~range:eligible_pairs (fun (bs, cs) -> r' bs +&& s' cs)
        

    method build_LProj env s' r' tuple =
      r' tuple +&& (s' @@ Tuple.(of_list1 [ith 0 tuple]))

    method build_None_ env = fun _ -> false_

    method build_Over env r' s' tuple =
      s' tuple +|| (r' tuple +&& (not_ @@ s' Tuple.(of_list1 [ith 0 tuple])))

    method build_Prime env = failwith "build_Prime"

    method build_Prod env = failwith "build_Prod" (* need to compute arity of r and s *)

    method build_RIte env f_r e1_r e2_r tuple =
      f_r @=> e1_r tuple +&& not_ f_r @=> e2_r tuple

    method build_RNo env = assert false

    method build_ROne env = assert false

    method build_RProj env r' s' tuple =
      let lg = Tuple.arity tuple in
      r' tuple +&& (s' @@ Tuple.of_list1 [Tuple.ith (lg - 1) tuple])

    method build_RSome env = assert false

    method build_RTClos env = assert false

    method build_RUn env op e_r = op e_r

    method build_TClos env = failwith "build_TClos"

    method build_Transpose env r' tuple =
      assert (Tuple.arity tuple = 2);
      r' @@ Tuple.transpose tuple

    method build_Univ env = fun _ -> true_
    
    (* iexp *)

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
