open Containers


module Logic = struct
  module Atom = struct
    type t = string

    let pp out at =
      Format.fprintf out "%s" at

    let make name atoms =
      let ats = Tuple.to_list atoms in
      Format.sprintf "%a_%a" Name.pp name Fmtc.(list ~sep:underscore Atom.pp) ats


    module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
    include P 
  end

  module Ltl = SMV.MakePrintableLTL(Atom)
  include Ltl
end

module FormulaConverter = Elo_to_LTL.MakeLtlConverter(Logic)

open FormulaConverter

let run elo =
  let open Elo in
  Logic.conj @@ List.map (convert elo) elo.goals 

let transfo = Transfo.make "to_smv1" run
