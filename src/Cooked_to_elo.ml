
open Containers

module E = Elo_goal

let convert_arity = function
  | None -> 0
  | Some n when n > 0 -> n
  | Some _ -> assert false

let convert 
      (fml : (Elo.var, Elo.ident) GenGoal.fml) : E.fml = 
  let converter = object
    inherit [_] GenGoal.fold
    method build_Add __env = 
      E.add
    method build_All __env = 
      E.all
    method build_And __env = 
      E.and_
    method build_Block __env fs' = 
      E.block fs'
    method build_BoxJoin __env = 
      assert false (* simplified *)
    method build_Card __env e' = 
      E.card e'
    method build_Compr __env = 
      failwith "TODO"
    method build_Diff __env = 
      E.diff
    method build_F __env = 
      E.sometime
    method build_FIte __env = 
      failwith "TODO"
    method build_False __env = 
      E.false_
    method build_G __env = 
      E.always
    method build_Gt __env = 
      E.gt
    method build_Gte __env = 
      E.gte
    method build_H __env = 
      E.historically
    method build_IBin __env = 
      failwith "TODO"
    method build_IComp __env = 
      failwith "TODO"
    method build_IEq __env = 
      E.ieq
    method build_INEq __env = 
      E.ineq
    method build_IUn __env = 
      failwith "TODO"
    method build_Iden __env = 
      E.iden
    method build_Ident env = function
      | Elo.Name n -> E.name ~ar:(failwith "TODO") n
      | Elo.Var v -> 
          E.var ~ar:(failwith "TODO") @@ List.assoc ~eq:Var.equal v env
    method build_Iff __env = 
      E.iff
    method build_Imp __env = 
      E.impl
    method build_In __env = 
      E.in_
    method build_Inter __env = 
      E.inter
    method build_Join __env = 
      E.join
    method build_LBin __env = 
      failwith "TODO"
    method build_LProj __env = 
      E.lproj
    method build_LUn __env = 
      failwith "TODO"
    method build_Let __env = 
      assert false (* simplified *)
    method build_Lone __env = 
      assert false (* simplified *)
    method build_Lt __env = 
      E.lt
    method build_Lte __env = 
      E.lte
    method build_Neg __env = 
      E.neg
    method build_No __env = 
      E.no_
    method build_None_ __env = 
      E.none
    method build_Not __env = 
      E.not_
    method build_NotIn __env = 
      E.not_in
    method build_Num __env = 
      failwith "TODO"
    method build_O __env = 
      E.once
    method build_One __env = 
      assert false (* simplified *)
    method build_Or __env = 
      E.or_
    method build_Over __env = 
      E.over
    method build_P __env = 
      E.previous
    method build_Prime __env = 
      failwith "TODO"
    method build_Prod __env = 
      E.prod
    method build_Qual __env = 
      failwith "TODO"
    method build_Quant __env = 
      failwith "TODO"
    method build_R __env = 
      E.releases
    method build_RBin __env = 
      failwith "TODO"
    method build_RComp __env = 
      failwith "TODO"
    method build_REq __env = 
      E.req
    method build_RIte __env = 
      failwith "TODO"
    method build_RLone __env = 
      assert false (* simplified *)
    method build_RNEq __env = 
      E.rneq
    method build_RNo __env = 
      assert false (* simplified *)
    method build_ROne __env = 
      assert false (* simplified *)
    method build_RProj __env = 
      E.lproj
    method build_RSome __env = 
      assert false (* simplified *)
    method build_RTClos __env = 
      E.rtclos
    method build_RUn __env = 
      failwith "TODO"
    method build_Run __env = 
      failwith "TODO"
    method build_S __env = 
      E.since
    method build_Some_ __env = 
      failwith "TODO"
    method build_Sub __env = 
      E.sub
    method build_TClos __env = 
      E.tclos
    method build_Transpose __env = 
      E.transpose
    method build_True __env = 
      E.true_
    method build_U __env = 
      E.until
    method build_Union __env = 
      E.union
    method build_Univ __env = 
      E.univ
    method build_X __env = 
      E.next
    method build_exp __env e' __loc __ar = 
      e'   (* no prim_exp in Elo_goal so e' is already an exp *)
    method build_fml __env f' __loc = 
      f'
    method build_iexp __env ie' __loc = 
      ie'
    method visit_'i __env ident = 
      ident
    method visit_'v __env var = 
      var
  end
  in converter # visit_fml [] fml