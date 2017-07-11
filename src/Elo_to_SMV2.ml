open Containers


module SMV_LTL = Elo_to_SMV1.SMV_LTL

module FormulaConverter = Elo_to_LTL2.Make(SMV_LTL)

open FormulaConverter

let run elo =
  let open Elo in
  convert elo

let transfo = Transfo.make "to_smv2" run
