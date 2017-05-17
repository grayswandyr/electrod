open Containers


module Logic = Elo_to_SMV1.Logic

module FormulaConverter = Elo_to_LTL2.MakeLtlConverter(Logic)

open FormulaConverter

let run elo =
  let open Elo in
  convert elo

let transfo = Transfo.make "to_smv2" run
