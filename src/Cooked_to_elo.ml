
open Containers

module E = Elo_goal

let convert_arity = function
  | None -> 0
  | Some n when n > 0 -> n
  | Some _ -> assert false

let rec convert_fml stack ({ prim_fml; _ }: (Elo.var, Elo.ident) GenGoal.fml) =
  match prim_fml with
    | Qual (_, _) -> assert false (* simplified *)
    | Let (_, _) -> assert false (* simplified *)
    | Quant (_, _::_::_, _) -> assert false (* simplified *)
    | Quant (_, [], _) -> assert false (* impossible *)
    | Quant (q, [disj, vars, range], block) -> 
        let range' = convert_exp stack range in
        (* let new_env = make_subst vars stack.length in
           let block' = convert_block (append new_env stack) block in *)
        let new_env = stack @ List.rev_map (function Elo.BVar v -> v) vars in
        let block' = convert_block (new_env @ stack) block in
        E.quant (convert_quant q) (disj, List.length vars, range') block'
    | True -> E.true_
    | False -> E.false_
    | RComp (e1, comp, e2) -> 
        E.rcomp (convert_exp stack e1) 
          (convert_comp_op comp) (convert_exp stack e2)
    | IComp (e1, comp, e2) ->           
        E.icomp (convert_iexp stack e1) 
          (convert_icomp_op comp) (convert_iexp stack e2)
    | LUn (op, f) -> 
        E.lunary (convert_lunop op) (convert_fml stack f)
    | LBin (f1, op, f2) -> 
        E.lbinary (convert_fml stack f1) (
          convert_lbinop op) (convert_fml stack f2)
    | FIte (c, t, e) -> 
        E.fite (convert_fml stack c) (convert_fml stack t) (convert_fml stack e)
    | Block fmls -> 
        E.block @@ convert_block stack fmls

and convert_block stack fmls = 
  List.map (convert_fml stack) fmls

and convert_quant (q : GenGoal.quant) = match q with
  | All -> E.all
  | Some_ -> E.some
  | No -> E.no_
  | One | Lone -> assert false (* simplified *)

and convert_comp_op (comp : GenGoal.comp_op) = match comp with
  | In -> E.in_
  | NotIn -> E.not_in
  | REq -> E.req
  | RNEq -> E.rneq

and convert_icomp_op (comp : GenGoal.icomp_op) = match comp with
  | IEq -> E.ieq
  | INEq -> E.ineq
  | Lt -> E.lt
  | Lte -> E.lte
  | Gt -> E.gt
  | Gte -> E.gte

and convert_lunop (op : GenGoal.lunop) = match op with
  | F -> E.sometime
  | G -> E.always
  | Not -> E.not_
  | O -> E.once
  | X -> E.next
  | H -> E.historically
  | P -> E.previous

and convert_lbinop (op : GenGoal.lbinop) = match op with
  | And -> E.and_
  | Or -> E.or_
  | Imp -> E.impl
  | Iff -> E.iff
  | U -> E.until
  | R -> E.releases
  | S -> E.since

and get_var v stack = 
  1 + (fst @@ Option.get_exn @@ List.find_idx (fun var -> Var.equal v var) stack)

and convert_exp stack 
      ({ prim_exp; arity; _ } : (Elo.var, Elo.ident) GenGoal.exp) =
  let ar = convert_arity arity in
  match prim_exp with
    | None_ -> E.none
    | Univ -> E.univ
    | Iden -> E.iden
    | Ident (Var v) -> (* E.var ~ar @@ List.assoc ~eq:Var.equal v stack.subst *)
        E.var ~ar @@ get_var v stack
    | Ident (Name n) -> E.name ~ar n
    | RUn (op, e) -> E.runary ~ar (convert_runop op) (convert_exp stack e)
    | RBin (e1, op, e2) -> 
        E.rbinary ~ar (convert_exp stack e1) 
          (convert_rbinop op) (convert_exp stack e2)
    | RIte (c, t, e) -> 
        E.rite ~ar (convert_fml stack c) 
          (convert_exp stack t) (convert_exp stack e)
    | BoxJoin (_, _) -> assert false (* simplified *)
    | Prime e -> E.prime ~ar @@ convert_exp stack e
    | Compr ([], _) -> assert false (* impossible *)
    | Compr (_::_::_, _) -> assert false (* simplified *)
    | Compr ([(disj, vars, range)], block) -> 
        (*        let range' = convert_exp stack range in
                  let new_env = make_subst vars stack.length in
                  let block' = convert_block (append new_env stack) block in
                  E.compr ~ar (disj, new_env.length, range') block' *)
        let range' = convert_exp stack range in
        (* let new_env = make_subst vars stack.length in
           let block' = convert_block (append new_env stack) block in *)
        let new_env = stack @ List.rev_map (function Elo.BVar v -> v) vars in
        let block' = convert_block (new_env @ stack) block in
        E.compr ~ar (disj, List.length vars, range') block'

and convert_runop (op : GenGoal.runop) = match op with
  | Transpose -> E.transpose
  | TClos -> E.tclos
  | RTClos -> E.rtclos

and convert_rbinop (op : GenGoal.rbinop) = match op with
  | Union -> E.union
  | Inter -> E.inter
  | Over -> E.over
  | LProj -> E.lproj
  | RProj -> E.rproj
  | Prod -> E.prod
  | Diff -> E.diff
  | Join -> E.join

and convert_iexp stack 
      ({ prim_iexp; _ } : (Elo.var, Elo.ident) GenGoal.iexp) = 
  match prim_iexp with
    | Num n -> E.num n
    | Card e -> E.card @@ convert_exp stack e
    | IUn (Neg, e) -> E.iunary E.neg @@ convert_iexp stack e
    | IBin (e1, op, e2) -> 
        E.ibinary (convert_iexp stack e1) 
          (convert_ibinop op) (convert_iexp stack e2)

and convert_ibinop (op : GenGoal.ibinop) = match op with
  | Add -> E.add
  | Sub -> E.sub

let convert_goal (GenGoal.Run fmls) = 
  E.run @@ convert_block [] fmls


module Test = struct
  let%test _ =
    let open E in
    let t = rbinary ~ar:1 (var ~ar:1 1) join (name ~ar:2 @@ Name.name "r") in
    let u = rbinary ~ar:1 (var ~ar:1 1) join (name ~ar:2 @@ Name.name "r") in
    Pervasives.(t == u)

  let%expect_test _ =
    let g = 
      let open E in
      quant some (sim_binding false 1 univ)
        [quant all (sim_binding true 2 @@ univ)
           [rcomp (var ~ar:1 1) in_ univ]] in
    let s = Fmt.to_to_string (E.pp_fml 0) g in 
    Printf.printf "%s" s;
    [%expect{| (some v/1 : univ {(all disj v/2, v/3 : univ {(v/3 in univ)})}) |}]

  open GenGoal
  open Elo
  let x = Var.fresh "x"
  let y = Var.fresh "y"
  let f : (Elo.var, Elo.ident) fml =
    fml Location.dummy @@ quant all [ (true, [bound_var x; bound_var y], exp (Some 1) Location.dummy univ)] [ fml Location.dummy @@ rcomp (exp (Some 1) Location.dummy @@ ident @@ var_ident x) in_ (exp (Some 1) Location.dummy univ)]
  let g : (Elo.var, Elo.ident) fml =
    fml Location.dummy @@ quant all [ (true, [bound_var x; bound_var y], exp (Some 1) Location.dummy univ)] [ fml Location.dummy @@ rcomp (exp (Some 1) Location.dummy @@ ident @@ var_ident x) in_ (exp (Some 1) Location.dummy univ)]

  let f' = Cooked_to_elo.(convert_fml []) f
  let g' = Cooked_to_elo.(convert_fml []) g

  let%test _ =
    Pervasives.(f' == g')


  let%expect_test _ =
    let fs = Fmt.to_to_string (E.pp_fml 0) f' in 
    let gs = Fmt.to_to_string (E.pp_fml 0) f' in 
    Printf.printf "%s" fs;
    [%expect{| (all disj v/1, v/2 : univ {(v/1 in univ)}) |}];
    Printf.printf "%s" gs;
    [%expect{| (all disj v/1, v/2 : univ {(v/1 in univ)}) |}]


end
