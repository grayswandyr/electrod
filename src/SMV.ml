open Containers


module MakePrintableLTL (At : LTL.ATOM) : LTL.PrintableLTL = struct
  module I = LTL.LTL_from_Atom(At) 

  include I

  module PP = struct
    open Fmtc

    type atom = At.t

    let pp_atom = Fmtc.(styled Name.style) At.pp

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
        | Not p -> prefix string pp out ("!", p)
        | And (p, q) -> infix string pp pp out ("&", p, q)
        | Or (p, q)-> infix string pp pp out ("|", p, q)
        | Imp (p, q)-> infix string pp pp out ("->", p, q)
        | Iff (p, q)-> infix string pp pp out ("<->", p, q)
        | Xor (p, q)-> infix string pp pp out ("xor", p, q)
        | Ite (c, t, e) -> pf out "@[<hov 2>(%a@ ?@ %a@ :@ %a)@]" pp c pp t pp e
        | X p -> prefix string pp out ("X ", p)
        | F p -> prefix string pp out ("F ", p)
        | G p -> prefix string pp out ("G ", p)
        | Y p -> prefix string pp out ("Y ", p)
        | O p -> prefix string pp out ("O ", p)
        | H p -> prefix string pp out ("H ", p)
        | U (p, q)-> infix string pp pp out ("U", p, q)
        | R (p, q)-> infix string pp pp out ("V", p, q)
        | S (p, q)-> infix string pp pp out ("S", p, q)
        | T (p, q)-> infix string pp pp out ("T", p, q)

    and pp_term out (t : term) = match t with
      | Num n -> pf out "%d" n
      | Plus (t1, t2) -> infix string pp_term pp_term out ("+", t1, t2)
      | Minus (t1, t2) -> infix string pp_term pp_term out ("-", t1, t2)
      | Neg t -> prefix string pp_term out ("- ", t)
      | Count ts ->
          pf out "@[count(@[<hov 2>%a@])@]"
            (list ~sep:(const string "+") pp) ts
  end


  let pp = PP.pp

  module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
  include P 


end

module File (Logic : LTL.PrintableLTL) = struct

  (* TODO file format *)
end
