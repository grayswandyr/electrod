open Containers


module SMV_atom : Solver.ATOMIC_PROPOSITION = struct
  type t = string

  let pp out at =
    Format.fprintf out "%s" at

  let make name atoms =
    let ats = Tuple.to_list atoms in
    Format.sprintf "%a_%a" Name.pp name Fmtc.(list ~sep:underscore Atom.pp) ats

  let compare = String.compare 

  module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
  include P 
end

module SMV_LTL = SMV.Make_SMV_LTL(SMV_atom)

module SMV_file_format = SMV.Make_SMV_file_format(SMV_LTL)

module Elo_to_SMV_LTL = Elo_to_LTL1.Make(SMV_LTL)

module Elo_to_SMV_model = Elo_to_model.Make(Elo_to_SMV_LTL)(SMV_file_format)

let pp = SMV_file_format.pp
           
(* temporary *)
let run elo =
  Elo_to_SMV_model.run elo

let transfo = Transfo.make "to_smv1" run
