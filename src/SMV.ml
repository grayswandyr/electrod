open Containers


module MakeLTL (At : LTL.ATOM) = struct
  module I = LTL.LTL_from_Atom(At) 

  include I
      
  module PP = struct
    open Fmtc
        
    type atom = At.t

    let pp_atom = At.pp

    let pp_tcomp out (t : tcomp) =
      pf out "%s"
      @@ match t with
      | Lte  -> "<="
      | Lt  -> "<"
      | Gte  -> ">="
      | Gt  -> ">"
      | Eq  -> "="
      | Neq  -> "!="
    
    let rec pp out f =
      match f with
        | Comp (op, t1, t2) -> infix pp_tcomp pp_term pp_term out (op, t1, t2)
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

    and pp_term out (t : term) = match t with
      | Num n -> pf out "%d" n
      | Plus (t1, t2) -> infix string pp_term pp_term out ("+", t1, t2)
      | Minus (t1, t2) -> infix string pp_term pp_term out ("-", t1, t2)
      | Neg t -> pf out "@[(- %a)@]" pp_term t
      | Count ts ->
          pf out "@[count(@[<hov 2>%a@])@]"
            (list ~sep:(const string "@ +@ ") pp) ts
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
