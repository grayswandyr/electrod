open Containers


module Atom = struct
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

module Logic = SMV.MakePrintableLTL(Atom)

module FormulaConverter = Elo_to_LTL1.MakeLtlConverter(Logic)

module ModelConverter = Elo_to_model.Make (Logic) (FormulaConverter) (SMV.MakeFile)

let pp = ModelConverter.ModelFormat.pp
           
(* temporary *)
let run elo =
  ModelConverter.run elo

let transfo = Transfo.make "to_smv1" run
