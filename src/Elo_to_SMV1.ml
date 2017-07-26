(** Provides a transformation from Electrod models to SMV models.  *)

open Containers

  
module SMV_atom : Solver.ATOMIC_PROPOSITION = struct
  type t = Symbol.t 
    
  let compare = Symbol.compare

  let pp = Symbol.pp

  let equal = Symbol.equal

  let hash = Symbol.hash


  (* table tracking which pair (name, tuple) a string comes from. Uses
     hahsconsing to make this more efficient *)
  module HT = Hashtbl.Make(Symbol)
      
  let names_and_tuples = HT.create 297
             
  let rel_sep = "$"

  let atom_sep = Fmtc.minus
  
  let make name atoms =
    let ats = Tuple.to_list atoms in
    Symbol.make @@
    Format.sprintf "%a%s%a"
      Name.pp name
      rel_sep
      Fmtc.(list ~sep:atom_sep Atom.pp) ats
    (* keep trace of creations to allow to get original pairs back *)
    |> Fun.tap (fun hs -> HT.add names_and_tuples hs (name, atoms))

  
  let split str =
    HT.find names_and_tuples @@ Symbol.make str
   
end

module SMV_LTL = SMV.Make_SMV_LTL(SMV_atom)

module SMV_file_format = SMV.Make_SMV_file_format(SMV_LTL)

module Elo_to_SMV_LTL = Elo_to_LTL1.Make(SMV_LTL)

module Elo_to_SMV_model = Elo_to_model1.Make(SMV_LTL)(Elo_to_SMV_LTL)(SMV_file_format)

let pp = SMV_file_format.pp

let analyze = SMV_file_format.analyze
           
(* temporary *)
let run elo =
  Elo_to_SMV_model.run elo

let transfo = Transfo.make "to_smv1" run
