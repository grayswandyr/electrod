(** Provides a transformation from Electrod models to SMV models.  *)

open Containers

  
module SMV_atom : Solver.ATOMIC_PROPOSITION = struct
  module H = Hashcons.Make(struct
      type t = string
      let hash = String.hash
		  let equal = String.equal 
    end)
  
  type t = string Hashcons.hash_consed

  (* table for hashconsing *)
  let ht = H.create 297

  (* table keeping trace of which pair (name, tuple) a string comes. Uses
     hahsconsing to make this more efficient *)
  module HT = Hashtbl.Make(struct
      type nonrec t = t
      let hash x = x.Hashcons.tag
      let equal x1 x2 = x1.Hashcons.tag = x2.Hashcons.tag
    end)
      
  let names_and_tuples = HT.create 297
             
  let rel_sep = "$"

  let atom_sep = Fmtc.minus
  
  let make_aux name atoms =
    let ats = Tuple.to_list atoms in
    H.hashcons ht @@
    Format.sprintf "%a%s%a"
      Name.pp name
      rel_sep
      Fmtc.(list ~sep:atom_sep Atom.pp) ats

  let make name atoms =
    make_aux name atoms 
    (* keep trace of creations to get original pairs back *)
    |> Fun.tap (fun hs -> HT.add names_and_tuples hs (name, atoms))

  
  let split str =
    HT.find names_and_tuples @@ H.hashcons ht str
    
    

  let compare s1 s2 =
    let open Hashcons in
    Int.compare s1.tag s2.tag

  let pp out at =
    let open Hashcons in
    Format.fprintf out "%s" at.node 
   
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
