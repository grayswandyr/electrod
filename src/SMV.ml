open Containers


module Make_SMV_LTL (At : Solver.ATOMIC_PROPOSITION)
  : Solver.LTL with type atomic = At.t = struct
  module I = Solver.LTL_from_Atomic(At) 

  include I

  module PP = struct
    open Fmtc


    (* From NuXmv documentation, from high to low (excerpt, some precedences are ignored because they are not used)

       !

       - (unary minus)

       + -

       = != < > <= >=

       &

       | xor xnor

       (... ? ... : ...)

       <->

       -> (the only right associative op)

    
NOTE: precedences for LTL connectives are not specified, hence we force parenthesising of these.
*)

    
    let rainbow =
      let r = ref 0 in
      fun () ->
        let cur = !r in
        incr r;
        match cur with
          | 0 -> `Magenta
          | 1 -> `Yellow
          | 2 -> `Cyan
          | 3 -> `Green
          | 4 -> `Red
          | 5 -> r := 0; `Blue
          | _ -> assert false

    (* [upper] is the precedence of the context we're in, [this] is the priority
       for printing to do, [pr] is the function to make the printing of the expression *)
    let rainbow_paren ?(paren = false) ?(align_par = true)
          upper this out pr =
      (* parenthesize if specified so or if  forced by the current context*)
      let par = paren || this < upper in
      (* if parentheses are specified, they'll be numerous so avoid alignment  of closing parentheses *)
      let align_par = not paren && align_par in
      if par then               (* add parentheses *)
        let color = rainbow () in
        if align_par then
          Format.pp_open_box out 0
        else
          Format.pp_open_box out 2;
        styled color string out "(";
        if align_par then Format.pp_open_box out 2;
        (* we're adding parenthese so precedence goes back to 0 inside of them *)
        pr 0;
        if align_par then 
          begin
            Format.pp_close_box out ();
            cut out ()
          end ;
        styled color string out ")";
        Format.pp_close_box out ()
      else                      (* no paremtheses *)
        (* so keep [this] precedence *)
        pr this
      
    let infixl ?(paren = false) ?(align_par = true)
          upper this middle left right out (m, l, r) =
      rainbow_paren ~paren ~align_par upper this out @@
      fun new_this ->           (* new_this is this or 0 if parentheses were added *)
      begin
        left new_this out l;
        sp out ();
        styled `Bold middle out m;
        sp out ();
        right (new_this + 1) out r (* left associativity => increment the precedence *)
      end
                    

    let infixr ?(paren = false) ?(align_par = true)
          upper this middle left right out (m, l, r) =
      rainbow_paren ~paren ~align_par upper this out @@
      fun new_this ->
      begin
        left (new_this + 1) out l;
        sp out ();
        styled `Bold middle out m;
        sp out ();
        right new_this out r
      end

    let infixn ?(paren = false) ?(align_par = true)
          upper this middle left right out (m, l, r) =
      rainbow_paren ~paren ~align_par upper this out @@
      fun new_this ->
      begin
        left (new_this + 1) out l;
        sp out ();
        styled `Bold middle out m;
        sp out ();
        right (new_this + 1) out r
      end

    let prefix ?(paren = false) ?(align_par = true)
          upper this pprefix pbody out (prefix, body) =
      rainbow_paren ~paren ~align_par upper this out @@
      fun new_this ->
      begin
        styled `Bold pprefix out prefix;
        pbody (new_this + 1) out body
      end
                    
    type atomic = At.t

    let pp_atomic = At.pp

    let pp_tcomp out (t : tcomp) =
      pf out "%s"
      @@ match t with
      | Lte  -> "<="
      | Lt  -> "<"
      | Gte  -> ">="
      | Gt  -> ">"
      | Eq  -> "="
      | Neq  -> "!="

    let styled_parens st ppv_v out v =
      surround (styled st lparen) (styled st rparen) ppv_v out v
    
    let rec pp upper out f =
      assert (upper >= 0);
      match f with
        | True  -> pf out "TRUE"
        | False  -> pf out "FALSE"
        | Atomic at -> pf out "%a" pp_atomic at
        (* tweaks, here, to force parenthese around immediate subformulas of Imp
           and Iff as their precedence may not be easily remembered*)
        | Imp (p, q) ->
            let c = rainbow () in
            infixr ~paren:false upper 1 string
              (fun _ -> styled_parens c @@ bbox2 @@ pp 0)
              (fun _ -> styled_parens c @@ bbox2 @@ pp 0) out ("->", p, q)
        | Iff (p, q) ->
            let c = rainbow () in
            infixl ~paren:false upper 2
              string
              (fun _ -> styled_parens c @@ bbox2 @@ pp 0)
              (fun _ -> styled_parens c @@ bbox2 @@ pp 0) out ("<->", p, q)
        | Ite (c, t, e) ->
            pf out "(%a@ ?@ %a@ :@ %a)" (pp 3) c (pp 3) t (pp 3) e
        | Or (p, q) -> infixl ~paren:false upper 4 string pp pp out ("|", p, q)
        (* force parenthses as we're not used to see the Xor connective and so its precedence may be unclear *)
        | Xor (p, q) -> infixl ~paren:true upper 4 string pp pp out ("xor", p, q)
        | And (p, q) -> infixl ~paren:false upper 5 string pp pp out ("&", p, q)
        | Comp (op, t1, t2) ->
            infixn upper 6 pp_tcomp pp_term pp_term out (op, t1, t2)
        | Not p -> prefix upper 9 string pp out ("!", p)
        (* no known precedence for temporal operators so we force parenthses and
           use as the "this" precedence that of the upper context*)
        | U (p, q) -> infixl ~paren:true upper upper string pp pp out ("U", p, q)
        | R (p, q) -> infixl ~paren:true upper upper string pp pp out ("V", p, q)
        | S (p, q) -> infixl ~paren:true upper upper string pp pp out ("S", p, q)
        | T (p, q) -> infixl ~paren:true upper upper string pp pp out ("T", p, q)
        | X p -> prefix ~paren:true upper upper string pp out ("X ", p)
        | F p -> prefix ~paren:true upper upper string pp out ("F ", p)
        | G p -> prefix ~paren:true upper upper string pp out ("G ", p)
        | Y p -> prefix ~paren:true upper upper string pp out ("Y ", p)
        | O p -> prefix ~paren:true upper upper string pp out ("O ", p)
        | H p -> prefix ~paren:true upper upper string pp out ("H ", p)

    and pp_term upper out (t : term) = match t with
      | Num n -> pf out "%d" n
      | Plus (t1, t2) ->
          infixl ~paren:false upper 7 string pp_term pp_term out ("+", t1, t2)
      | Minus (t1, t2) ->
          infixl ~paren:false upper 7 string pp_term pp_term out ("-", t1, t2)
      | Neg t -> prefix upper 8 string pp_term out ("- ", t)
      | Count ts ->
          pf out "@[count(%a@])" (list ~sep:(const string "+") (pp 0)) ts
  end

  let pp_atomic = PP.pp_atomic

  let pp out f =
    Fmtc.pf out "@[<hov2>%a@]@." (PP.pp 0) f

end

module Make_SMV_file_format (Ltl : Solver.LTL)
  : Solver.MODEL with type ltl = Ltl.t and type atomic = Ltl.atomic = struct

  type ltl = Ltl.t

  type atomic = Ltl.atomic

  type t = {
    rigid : atomic Sequence.t;
    flexible : atomic Sequence.t;    
    invariant : ltl Sequence.t;
    property : ltl 
  }

  let make ~rigid ~flexible ~invariant ~property =
    { rigid; flexible; invariant; property }

  let pp_var_decl out atomic =
    Fmtc.pf out "%a : boolean;" Ltl.pp_atomic atomic

  let pp_invar out inv = 
    Fmtc.pf out 
      "----------------------------------------------------------------------@\n\
       INVAR@\n\
       ----------------------------------------------------------------------@\n\
       %a@\n"
      Ltl.pp inv
      
  let pp ?(margin = 78) out { rigid; flexible; invariant; property } =
    let open Fmtc in
    let module S = Sequence in
    let old_margin = Format.pp_get_margin out () in
    Format.pp_set_margin out margin;
    let rigid = Sequence.sort_uniq ~cmp:Ltl.compare_atomic rigid in
    let flexible = Sequence.sort_uniq ~cmp:Ltl.compare_atomic flexible in
    pf out
      "MODULE main@\n\
       JUSTICE TRUE;@\n\
       ----------------------------------------------------------------------@\n\
       FROZENVAR@\n\
       ----------------------------------------------------------------------@\n\
       %a@\n\
       ----------------------------------------------------------------------@\n\
       VAR@\n\
       ----------------------------------------------------------------------@\n\
       %a@\n\
       %a\
       ----------------------------------------------------------------------@\n\
       LTLSPEC@\n\
       ----------------------------------------------------------------------@\n\
       %a"
      (vbox @@ S.pp_seq ~sep:"" pp_var_decl) rigid
      (vbox @@ S.pp_seq ~sep:"" pp_var_decl) flexible
      (vbox @@ S.pp_seq ~sep:"" pp_invar) invariant
      Ltl.pp property;
    Format.pp_set_margin out old_margin 
   
end
