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
        | Ite (c, t, e) -> pf out "(%a@ ?@ %a@ :@ %a)" pp c pp t pp e
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
          pf out "@[count(%a@])" (list ~sep:(const string "+") pp) ts
  end


  module PPP = struct
    open Fmtc

    let infixl ?(indent = 2) upper this middle left right out (m, l, r) =
      let par = this < upper in
      if par then string out "(";
      left this out l;
      sp out ();
      styled `Bold middle out m;
      sp out ();
      right this out r;
      if par then string out ")"

    let infixr ?(indent = 2) upper this middle left right out (m, l, r) =
      let par = this < upper in
      if par then string out "(";
      left this out l;
      sp out ();
      styled `Bold middle out m;
      sp out ();
      right this out r;
      if par then string out ")"

    let infixn ?(indent = 2) upper this middle left right out (m, l, r) =
      let par = this < upper in
      if par then string out "(";
      left this out l;
      sp out ();
      styled `Bold middle out m;
      sp out ();
      right this out r;
      if par then string out ")"

    let prefix ?(indent = 2) upper this pprefix pbody out (prefix, body) =
      let par = this <= upper in
      if par then string out "(";
      styled `Bold pprefix out prefix;
      pbody this out body;
      if par then string out ")"
                    
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

    let rec pp upper out f =
      match f with
        | True  -> pf out "true"
        | False  -> pf out "false"
        | Atom at -> pp_atom out at
        | Iff (p, q)-> infixl upper 1 string pp pp out ("<->", p, q)
        | Or (p, q)-> infixl upper 2 string pp pp out ("|", p, q)
        | Xor (p, q)-> infixl upper 2 string pp pp out ("xor", p, q)
        | Imp (p, q)-> infixr upper 3 string pp pp out ("->", p, q)
        | And (p, q) -> infixl upper 4 string pp pp out ("&", p, q)
        | U (p, q)-> infixl upper 5 string pp pp out ("U", p, q)
        | R (p, q)-> infixl upper 5 string pp pp out ("V", p, q)
        | S (p, q)-> infixl upper 5 string pp pp out ("S", p, q)
        | T (p, q)-> infixl upper 5 string pp pp out ("T", p, q)
        | Not p -> prefix upper 6 string pp out ("!", p)
        | X p -> prefix upper 6 string pp out ("X ", p)
        | F p -> prefix upper 6 string pp out ("F ", p)
        | G p -> prefix upper 6 string pp out ("G ", p)
        | Y p -> prefix upper 6 string pp out ("Y ", p)
        | O p -> prefix upper 6 string pp out ("O ", p)
        | H p -> prefix upper 6 string pp out ("H ", p)
        | Ite (c, t, e) ->
            pf out "(%a@ ?@ %a@ :@ %a)" (pp 0) c (pp 0) t (pp 4) e
        | Comp (op, t1, t2) -> infixn upper 7 pp_tcomp pp_term pp_term out (op, t1, t2)

    and pp_term upper out (t : term) = match t with
      | Num n -> pf out "%d" n
      | Plus (t1, t2) -> infixl upper 8 string pp_term pp_term out ("+", t1, t2)
      | Minus (t1, t2) -> infixl upper 8 string pp_term pp_term out ("-", t1, t2)
      | Neg t -> prefix upper 9 string pp_term out ("- ", t)
      | Count ts ->
          pf out "@[count(%a@])" (list ~sep:(const string "+") (pp 0)) ts
  end

  let pp out f = Fmtc.pf out "@[<hov2>%a@]" (PPP.pp 0) f

  module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
  include P 


end

module File (Logic : LTL.PrintableLTL) = struct

  (* TODO file format *)
end
