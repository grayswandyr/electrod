open Containers


module MakeLTL (At : LTL.ATOM) = struct
  module I = LTL.LTL_from_Atom(At) 

  include I
      
  module PP = struct
    type atom = At.t

    let pp_atom = At.pp
        
    let rec pp out (f : t) =
      let open Fmtc in
      match f with
        | True  -> pf out "true"
        | False  -> pf out "false"
        | Atom at -> pp_atom out at
        | And (p, q) -> infix string pp pp out ("&", p, q)
        | Or (p, q)-> infix string pp pp out ("|", p, q)
        | Imp (p, q)-> infix string pp pp out ("->", p, q)
        | Iff (p, q)-> infix string pp pp out ("<->", p, q)
        | Xor (p, q)-> infix string pp pp out ("xor", p, q)
        | Ite (c, t, e) -> pf out "@[<hov 2>(%a@ ?@ %a@ :@ %a)@]" pp c pp t pp e
        | X p -> pf out "@[(X %a)@]" pp p
        | F p -> pf out "@[(F %a)@]" pp p
        | G p -> pf out "@[(G %a)@]" pp p
        | Y p -> pf out "@[(Y %a)@]" pp p
        | O p -> pf out "@[(O %a)@]" pp p
        | H p -> pf out "@[(H %a)@]" pp p
        | U (p, q)-> infix string pp pp out ("U", p, q)
        | R (p, q)-> infix string pp pp out ("V", p, q)
        | S (p, q)-> infix string pp pp out ("S", p, q)
        | T (p, q)-> infix string pp pp out ("T", p, q)
  end
  

  let pp = PP.pp

end

module type SMV_LTL = sig
  include LTL.S
    (* TODO *)
end

module File (Logic : SMV_LTL) = struct

  (* TODO file format *)
end
