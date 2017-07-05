open Containers


module MakeLtlConverter (Ltl : LTL.S) = struct
  module M = Elo_to_LTL1.MakeLtlConverter(Ltl)
      
  open Ltl

  class ['env] converter = object (self : 'self)
    inherit ['env] M.converter as super

    method build_TClos (env : 'env) r r' = fun tuple ->
      failwith "build_TClos BRUNEL method 2 !!!"
  end

  class environment (elo : Elo.t) = object (self : 'self)
    inherit M.environment elo as super
      
    val mutable invar : Ltl.t list = []

    method add fml =
      invar <- fml :: invar
  end


  let convert elo =
    let open Elo in
    let env = new environment elo in
    let GenGoal.Sat fmls = elo.goal in
    List.map ((new converter)#visit_fml env) fmls
  
end
