(** Implements a recursor over generic goals (necessary for conversion
    to LTL).  *)

[@@@landmark "auto"]

open GenGoal

class virtual ['self] recursor = object (self : 'self)
  inherit [_] VisitorsRuntime.map
  method virtual build_Add : _
  method virtual build_All : _
  method virtual build_And : _
  method virtual build_Block : _
  method virtual build_BoxJoin : _
  method virtual build_Card : _
  method virtual build_Compr : _
  method virtual build_Diff : _
  method virtual build_F : _
  method virtual build_FIte : _
  method virtual build_False : _
  method virtual build_G : _
  method virtual build_Gt : _
  method virtual build_Gte : _
  method virtual build_H : _
  method virtual build_IBin : _
  method virtual build_IComp : _
  method virtual build_IEq : _
  method virtual build_INEq : _
  method virtual build_IUn : _
  method virtual build_Iden : _
  method virtual build_Ident : _
  method virtual build_Iff : _
  method virtual build_Imp : _
  method virtual build_In : _
  method virtual build_Inter : _
  method virtual build_Join : _
  method virtual build_LBin : _
  method virtual build_LProj : _
  method virtual build_LUn : _
  method virtual build_Let : _
  method virtual build_Lone : _
  method virtual build_Lt : _
  method virtual build_Lte : _
  method virtual build_Neg : _
  method virtual build_No : _
  method virtual build_None_ : _
  method virtual build_Not : _
  method virtual build_NotIn : _
  method virtual build_Num : _
  method virtual build_O : _
  method virtual build_One : _
  method virtual build_Or : _
  method virtual build_Over : _
  method virtual build_P : _
  method virtual build_Prime : _
  method virtual build_Prod : _
  method virtual build_Qual : _
  method virtual build_Quant : _
  method virtual build_R : _
  method virtual build_RBin : _
  method virtual build_RComp : _
  method virtual build_REq : _
  method virtual build_RIte : _
  method virtual build_RLone : _
  method virtual build_RNEq : _
  method virtual build_RNo : _
  method virtual build_ROne : _
  method virtual build_RProj : _
  method virtual build_RSome : _
  method virtual build_RTClos : _
  method virtual build_RUn : _
  method virtual build_S : _
  method virtual build_Run : _
  method virtual build_Some_ : _
  method virtual build_Sub : _
  method virtual build_TClos : _
  method virtual build_Transpose : _
  method virtual build_True : _
  method virtual build_U : _
  method virtual build_Union : _
  method virtual build_Univ : _
  method virtual build_X : _
  method virtual build_exp : _
  method virtual build_fml : _
  method virtual build_iexp : _
  method virtual visit_'i : _
  method virtual visit_'v : _
  method visit_Run env _visitors_c0 =
    let _visitors_r0 = self#visit_list self#visit_fml env _visitors_c0  in
    self#build_Run env _visitors_c0 _visitors_r0
  method visit_t env _visitors_this =
    match _visitors_this with
      | Run _visitors_c0 -> self#visit_Run env _visitors_c0
  method visit_fml env _visitors_this =
    let _visitors_r0 = self#visit_prim_fml env _visitors_this.prim_fml
    in
    let _visitors_r1 =
      (fun _visitors_this -> _visitors_this) _visitors_this.fml_loc
    in
    self#build_fml env _visitors_this _visitors_r0 _visitors_r1 [@landmark "build_fml"]
  method visit_True env =
    self#build_True env [@landmark "build_True"]
  method visit_False env =
    self#build_False env [@landmark "build_False"]
  method visit_Qual env _visitors_c0 _visitors_c1 =
    let _visitors_r0 = self#visit_rqualify env _visitors_c0 in
    let _visitors_r1 = self#visit_exp env _visitors_c1 in
    self#build_Qual env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1 [@landmark "build_Qual"]
  method visit_RComp env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_exp env _visitors_c0 in
    let _visitors_r1 = self#visit_comp_op env _visitors_c1 in
    let _visitors_r2 = self#visit_exp env _visitors_c2 in
    self#build_RComp env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_RComp"]
  method visit_IComp env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_iexp env _visitors_c0 in
    let _visitors_r1 = self#visit_icomp_op env _visitors_c1 in
    let _visitors_r2 = self#visit_iexp env _visitors_c2 in
    self#build_IComp env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_IComp"]
  method visit_LUn env _visitors_c0 _visitors_c1 =
    let _visitors_r0 = self#visit_lunop env _visitors_c0 in
    let _visitors_r1 = self#visit_fml env _visitors_c1 in
    self#build_LUn env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1 [@landmark "build_LUn"]
  method visit_LBin env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_fml env _visitors_c0 in
    let _visitors_r1 = self#visit_lbinop env _visitors_c1 in
    let _visitors_r2 = self#visit_fml env _visitors_c2 in
    self#build_LBin env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_LBin"]
  method visit_Quant env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_quant env _visitors_c0 in
    let _visitors_r1 =
      self#visit_list self#visit_sim_binding env _visitors_c1 in
    let _visitors_r2 = self#visit_block env _visitors_c2 in
    self#build_Quant env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_Quant"]
  method visit_Let env _visitors_c0 _visitors_c1 =
    let _visitors_r0 =
      self#visit_list self#visit_binding env _visitors_c0 in
    let _visitors_r1 = self#visit_block env _visitors_c1 in
    self#build_Let env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1 [@landmark "build_Let"]
  method visit_FIte env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_fml env _visitors_c0 in
    let _visitors_r1 = self#visit_fml env _visitors_c1 in
    let _visitors_r2 = self#visit_fml env _visitors_c2 in
    self#build_FIte env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_FIte"]
  method visit_Block env _visitors_c0 =
    let _visitors_r0 = self#visit_block env _visitors_c0 in
    self#build_Block env _visitors_c0 _visitors_r0 [@landmark "build_Block"]
  method visit_prim_fml env _visitors_this =
    match _visitors_this with
      | True ->
          self#visit_True env
      | False ->
          self#visit_False env
      | Qual (_visitors_c0, _visitors_c1) ->
          self#visit_Qual env _visitors_c0 _visitors_c1
      | RComp (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_RComp env _visitors_c0 _visitors_c1 _visitors_c2
      | IComp (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_IComp env _visitors_c0 _visitors_c1 _visitors_c2
      | LUn (_visitors_c0, _visitors_c1) ->
          self#visit_LUn env _visitors_c0 _visitors_c1
      | LBin (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_LBin env _visitors_c0 _visitors_c1 _visitors_c2
      | Quant (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_Quant env _visitors_c0 _visitors_c1 _visitors_c2
      | Let (_visitors_c0, _visitors_c1) ->
          self#visit_Let env _visitors_c0 _visitors_c1
      | FIte (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_FIte env _visitors_c0 _visitors_c1 _visitors_c2
      | Block _visitors_c0 ->
          self#visit_Block env _visitors_c0
  method visit_binding env (_visitors_c0, _visitors_c1) =
    let _visitors_r0 = self#visit_'v env _visitors_c0 in
    let _visitors_r1 = self#visit_exp env _visitors_c1 in
    (_visitors_r0, _visitors_r1)
  method visit_sim_binding env (_visitors_c0, _visitors_c1, _visitors_c2)
    =
    let _visitors_r0 = self#visit_disj env _visitors_c0 in
    let _visitors_r1 = self#visit_list self#visit_'v env _visitors_c1
    in
    let _visitors_r2 = self#visit_exp env _visitors_c2 in
    (_visitors_r0, _visitors_r1, _visitors_r2)
  method visit_disj env _visitors_this =
    _visitors_this
  method visit_block env =
    self#visit_list self#visit_fml env
  method visit_All env =
    self#build_All env [@landmark "build_All"]
  method visit_Some_ env =
    self#build_Some_ env [@landmark "build_Some_"]
  method visit_No env =
    self#build_No env [@landmark "build_No"]
  method visit_One env =
    self#build_One env [@landmark "build_One"]
  method visit_Lone env =
    self#build_Lone env [@landmark "build_Lone"]
  method visit_quant env _visitors_this =
    match _visitors_this with
      | All ->
          self#visit_All env
      | Some_ ->
          self#visit_Some_ env
      | No ->
          self#visit_No env
      | One ->
          self#visit_One env
      | Lone ->
          self#visit_Lone env
  method visit_And env =
    self#build_And env [@landmark "build_And"]
  method visit_Or env =
    self#build_Or env [@landmark "build_Or"]
  method visit_Imp env =
    self#build_Imp env [@landmark "build_Imp"]
  method visit_Iff env =
    self#build_Iff env [@landmark "build_Iff"]
  method visit_U env =
    self#build_U env [@landmark "build_U"]
  method visit_R env =
    self#build_R env [@landmark "build_R"]
  method visit_S env =
    self#build_S env [@landmark "build_S"]
  method visit_lbinop env _visitors_this =
    match _visitors_this with
      | And ->
          self#visit_And env
      | Or ->
          self#visit_Or env
      | Imp ->
          self#visit_Imp env
      | Iff ->
          self#visit_Iff env
      | U ->
          self#visit_U env
      | R ->
          self#visit_R env
      | S ->
          self#visit_S env
  method visit_F env =
    self#build_F env [@landmark "build_F"]
  method visit_G env =
    self#build_G env [@landmark "build_G"]
  method visit_Not env =
    self#build_Not env [@landmark "build_Not"]
  method visit_O env =
    self#build_O env [@landmark "build_O"]
  method visit_X env =
    self#build_X env [@landmark "build_X"]
  method visit_H env =
    self#build_H env [@landmark "build_H"]
  method visit_P env =
    self#build_P env [@landmark "build_P"]
  method visit_lunop env _visitors_this =
    match _visitors_this with
      | F ->
          self#visit_F env
      | G ->
          self#visit_G env
      | Not ->
          self#visit_Not env
      | O ->
          self#visit_O env
      | X ->
          self#visit_X env
      | H ->
          self#visit_H env
      | P ->
          self#visit_P env
  method visit_In env =
    self#build_In env [@landmark "build_In"]
  method visit_NotIn env =
    self#build_NotIn env [@landmark "build_NotIn"]
  method visit_REq env =
    self#build_REq env [@landmark "build_REq"]
  method visit_RNEq env =
    self#build_RNEq env [@landmark "build_RNEq"]
  method visit_comp_op env _visitors_this =
    match _visitors_this with
      | In ->
          self#visit_In env
      | NotIn ->
          self#visit_NotIn env
      | REq ->
          self#visit_REq env
      | RNEq ->
          self#visit_RNEq env
  method visit_IEq env =
    self#build_IEq env [@landmark "build_IEq"]
  method visit_INEq env =
    self#build_INEq env [@landmark "build_INEq"]
  method visit_Lt env =
    self#build_Lt env [@landmark "build_Lt"]
  method visit_Lte env =
    self#build_Lte env [@landmark "build_Lte"]
  method visit_Gt env =
    self#build_Gt env [@landmark "build_Gt"]
  method visit_Gte env =
    self#build_Gte env [@landmark "build_Gte"]
  method visit_icomp_op env _visitors_this =
    match _visitors_this with
      | IEq ->
          self#visit_IEq env
      | INEq ->
          self#visit_INEq env
      | Lt ->
          self#visit_Lt env
      | Lte ->
          self#visit_Lte env
      | Gt ->
          self#visit_Gt env
      | Gte ->
          self#visit_Gte env
  method visit_exp env _visitors_this =
    let _visitors_r0 = self#visit_prim_exp env _visitors_this.prim_exp
    in
    let _visitors_r1 =
      (fun _visitors_this -> _visitors_this) _visitors_this.exp_loc
    in
    let _visitors_r2 =
      (fun _visitors_this -> _visitors_this) _visitors_this.arity in
    self#build_exp env _visitors_this _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_exp"]
  method visit_None_ env =
    self#build_None_ env [@landmark "build_None_"]
  method visit_Univ env =
    self#build_Univ env [@landmark "build_Univ"]
  method visit_Iden env =
    self#build_Iden env [@landmark "build_Iden"]
  method visit_Ident env _visitors_c0 =
    let _visitors_r0 = self#visit_'i env _visitors_c0 in
    self#build_Ident env _visitors_c0 _visitors_r0 [@landmark "build_Ident"]
  method visit_RUn env _visitors_c0 _visitors_c1 =
    let _visitors_r0 = self#visit_runop env _visitors_c0 in
    let _visitors_r1 = self#visit_exp env _visitors_c1 in
    self#build_RUn env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1 [@landmark "build_RUn"]
  method visit_RBin env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_exp env _visitors_c0 in
    let _visitors_r1 = self#visit_rbinop env _visitors_c1 in
    let _visitors_r2 = self#visit_exp env _visitors_c2 in
    self#build_RBin env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_RBin"]
  method visit_RIte env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_fml env _visitors_c0 in
    let _visitors_r1 = self#visit_exp env _visitors_c1 in
    let _visitors_r2 = self#visit_exp env _visitors_c2 in
    self#build_RIte env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_RIte"]
  method visit_BoxJoin env _visitors_c0 _visitors_c1 =
    let _visitors_r0 = self#visit_exp env _visitors_c0 in
    let _visitors_r1 = self#visit_list self#visit_exp env _visitors_c1
    in
    self#build_BoxJoin env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1 [@landmark "build_BoxJoin"]
  method visit_Compr env _visitors_c0 _visitors_c1 =
    let _visitors_r0 =
      self#visit_list self#visit_sim_binding env _visitors_c0 in
    let _visitors_r1 = self#visit_block env _visitors_c1 in
    self#build_Compr env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1 [@landmark "build_Compr"]
  method visit_Prime env _visitors_c0 =
    let _visitors_r0 = self#visit_exp env _visitors_c0 in
    self#build_Prime env _visitors_c0 _visitors_r0 [@landmark "build_Prime"]
  method visit_prim_exp env _visitors_this =
    match _visitors_this with
      | None_ ->
          self#visit_None_ env
      | Univ ->
          self#visit_Univ env
      | Iden ->
          self#visit_Iden env
      | Ident _visitors_c0 ->
          self#visit_Ident env _visitors_c0
      | RUn (_visitors_c0, _visitors_c1) ->
          self#visit_RUn env _visitors_c0 _visitors_c1
      | RBin (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_RBin env _visitors_c0 _visitors_c1 _visitors_c2
      | RIte (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_RIte env _visitors_c0 _visitors_c1 _visitors_c2
      | BoxJoin (_visitors_c0, _visitors_c1) ->
          self#visit_BoxJoin env _visitors_c0 _visitors_c1
      | Compr (_visitors_c0, _visitors_c1) ->
          self#visit_Compr env _visitors_c0 _visitors_c1
      | Prime _visitors_c0 ->
          self#visit_Prime env _visitors_c0
  method visit_ROne env =
    self#build_ROne env [@landmark "build_ROne"]
  method visit_RLone env =
    self#build_RLone env [@landmark "build_RLone"]
  method visit_RSome env =
    self#build_RSome env [@landmark "build_RSome"]
  method visit_RNo env =
    self#build_RNo env [@landmark "build_RNo"]
  method visit_rqualify env _visitors_this =
    match _visitors_this with
      | ROne ->
          self#visit_ROne env
      | RLone ->
          self#visit_RLone env
      | RSome ->
          self#visit_RSome env
      | RNo ->
          self#visit_RNo env
  method visit_Transpose env =
    self#build_Transpose env [@landmark "build_Transpose"]
  method visit_TClos env  =
    self#build_TClos env [@landmark "build_TClos"]
  method visit_RTClos env =
    self#build_RTClos env [@landmark "build_RTClos"]
  method visit_runop env _visitors_this =
    match _visitors_this with
      | Transpose ->
          self#visit_Transpose env
      | TClos ->
          self#visit_TClos env
      | RTClos ->
          self#visit_RTClos env
  method visit_Union env _visitors_c0 _visitors_c1 =
    self#build_Union env _visitors_c0 _visitors_c1 [@landmark "build_Union"]
  method visit_Inter env =
    self#build_Inter env [@landmark "build_Inter"]
  method visit_Over env =
    self#build_Over env [@landmark "build_Over"]
  method visit_LProj env =
    self#build_LProj env [@landmark "build_LProj"]
  method visit_RProj env =
    self#build_RProj env [@landmark "build_RProj"]
  method visit_Prod env =
    self#build_Prod env [@landmark "build_Prod"]
  method visit_Diff env =
    self#build_Diff env [@landmark "build_Diff"]
  method visit_Join env =
    self#build_Join env [@landmark "build_Join"]
  method visit_rbinop env _visitors_this =
    match _visitors_this with
      | Union ->
          self#visit_Union env
      | Inter ->
          self#visit_Inter env
      | Over ->
          self#visit_Over env
      | LProj ->
          self#visit_LProj env
      | RProj ->
          self#visit_RProj env
      | Prod ->
          self#visit_Prod env
      | Diff ->
          self#visit_Diff env
      | Join ->
          self#visit_Join env
  method visit_iexp env _visitors_this =
    let _visitors_r0 =
      self#visit_prim_iexp env _visitors_this.prim_iexp in
    let _visitors_r1 =
      (fun _visitors_this -> _visitors_this) _visitors_this.iexp_loc
    in
    self#build_iexp env _visitors_this _visitors_r0 _visitors_r1 [@landmark "build_iexp"]
  method visit_Num env _visitors_c0 =
    let _visitors_r0 =
      (fun _visitors_this -> _visitors_this) _visitors_c0 in
    self#build_Num env _visitors_c0 _visitors_r0 [@landmark "build_Num"]
  method visit_Card env _visitors_c0 =
    let _visitors_r0 = self#visit_exp env _visitors_c0 in
    self#build_Card env _visitors_c0 _visitors_r0 [@landmark "build_Card"]
  method visit_IUn env _visitors_c0 _visitors_c1 =
    let _visitors_r0 = self#visit_iunop env _visitors_c0 in
    let _visitors_r1 = self#visit_iexp env _visitors_c1 in
    self#build_IUn env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1 [@landmark "build_IUn"]
  method visit_IBin env _visitors_c0 _visitors_c1 _visitors_c2 =
    let _visitors_r0 = self#visit_iexp env _visitors_c0 in
    let _visitors_r1 = self#visit_ibinop env _visitors_c1 in
    let _visitors_r2 = self#visit_iexp env _visitors_c2 in
    self#build_IBin env _visitors_c0 _visitors_c1 _visitors_c2 _visitors_r0 _visitors_r1 _visitors_r2 [@landmark "build_IBin"]
  method visit_prim_iexp env _visitors_this =
    match _visitors_this with
      | Num _visitors_c0 ->
          self#visit_Num env _visitors_c0
      | Card _visitors_c0 ->
          self#visit_Card env _visitors_c0
      | IUn (_visitors_c0, _visitors_c1) ->
          self#visit_IUn env _visitors_c0 _visitors_c1
      | IBin (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_IBin env _visitors_c0 _visitors_c1 _visitors_c2
  method visit_Neg env =
    self#build_Neg env [@landmark "build_Neg"]
  method visit_iunop env _visitors_this =
    match _visitors_this with | Neg -> self#visit_Neg env
  method visit_Add env =
    self#build_Add env [@landmark "build_Add"]
  method visit_Sub env =
    self#build_Sub env [@landmark "build_Sub"]
  method visit_ibinop env _visitors_this =
    match _visitors_this with
      | Add ->
          self#visit_Add env
      | Sub ->
          self#visit_Sub env
end
