open Containers


module Make (Ltl : Solver.LTL) = struct
  module M = Elo_to_LTL1.Make(Ltl)
      
  open Ltl

  type atomic = Ltl.atomic

  type ltl = Ltl.t

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


  (* Converts an Elo formula to an LTL formula, gathering at the same time the
     rigid and flexible variables having appeared during the walk. *)
  let convert elo elo_fml =
    let open Elo in
    let env = new environment elo in
    let ltl_fml = (new converter)#visit_fml env elo_fml in
    let (rigid, flexible) = env#atoms in
    (rigid, flexible, ltl_fml)
  
end
