open Elo
open GenGoal

(** TODO add to interface ? *)
(*let pp_var out (`Var v) =
  Var.pp out v

  let pp_ident out = function
  | `Name n -> Fmtc.(styled `Cyan Name.pp) out n
  | `Var v as var -> Fmtc.(styled `Yellow pp_var) out var
*)

let rec handle_let_in_fml bind fml =
  { fml with data = handle_let_in_prim_fml bind fml.data }
and handle_let_in_exp bind exp =
  { exp with data = handle_let_in_prim_exp bind exp.data }
and handle_let_in_iexp bind iexp =
  { iexp with data = handle_let_in_prim_iexp bind iexp.data }
and handle_let_in_prim_fml bind p =
  match p with
    | Let (a, b) -> (*Msg.debug (fun m -> m "hit let ");*)
        let a = List.map (handle_let_in_bindings bind) a in
        handle_let_in_prim_fml (a@bind) (block b)
    | Block a -> block (List.map (handle_let_in_fml bind) a)
    | FBuiltin (s, l) -> fbuiltin s (List.map (handle_let_in_exp bind) l)
    | Qual (q, p) -> qual q (handle_let_in_exp bind p)
    | RComp (p1, c, p2) ->
        rcomp (handle_let_in_exp bind p1) c (handle_let_in_exp bind p2)
    | IComp (i1, c, i2) ->
        icomp (handle_let_in_iexp bind i1) c (handle_let_in_iexp bind i2)
    | LUn (op, p) -> lunary op (handle_let_in_fml bind p)
    | LBin (p1, op, p2) ->
        lbinary (handle_let_in_fml bind p1) op (handle_let_in_fml bind p2)
    | QAEN(a, b, c) ->
        let b = List.map (handle_let_in_sim_bindings bind) b in
        qaen a b (List.map (handle_let_in_fml bind) c)
    | QLO (a, b, c) ->
        let b = List.map (handle_let_in_bindings bind) b in
        qlo a b (List.map (handle_let_in_fml bind) c)
    | FIte (p1, p2, p3) ->
        fite (handle_let_in_fml bind p1) (handle_let_in_fml bind p2)
          (handle_let_in_fml bind p3)
    | True
    | False
      -> p
and handle_let_in_prim_exp bind expr =
  match expr with
    | Ident (`Var v) ->
        (*Msg.debug (fun m ->
          let open Fmtc in
          m "find %a in bindings %a" Elo.pp_var (`Var v)
          (list ~sep:(sp **> comma) @@
          pp_binding ~sep:colon pp_var pp_ident) bind);*)
        begin
          match CCList.Assoc.get ~eq:(fun (`Var a) (`Var b) -> Var.equal a b)
                  bind (`Var v) with
              None -> expr
            | Some exp -> exp.data
        end
    | Ident _ -> expr
    | RUn (c, p) -> runary c (handle_let_in_exp bind p)
    | RBin (p1, c, p2) ->
        rbinary (handle_let_in_exp bind p1) c (handle_let_in_exp bind p2)
    | RIte (f, p1, p2) -> rite (handle_let_in_fml bind f)
                            (handle_let_in_exp bind p1) (handle_let_in_exp bind p2)
    | BoxJoin (e, l) ->
        boxjoin (handle_let_in_exp bind e) (List.map (handle_let_in_exp bind) l)
    | Compr (sb, b) ->
        let sb = List.map (handle_let_in_sim_bindings bind) sb in
        compr sb (List.map (handle_let_in_fml bind) b)
    | Prime p -> prime (handle_let_in_exp bind p)
    | None_
    | Univ
    | Iden
      -> expr
and handle_let_in_prim_iexp bind p =
  match p with
    | Num _ -> p
    | Card e -> card (handle_let_in_exp bind e)
    | IUn (o, i) -> iunary o (handle_let_in_iexp bind i)
    | IBin (i1, o, i2) ->
        ibinary (handle_let_in_iexp bind i1) o (handle_let_in_iexp bind i2)
and handle_let_in_bindings bind (a, b) =
  (a, handle_let_in_exp bind b)
and handle_let_in_sim_bindings bind (a, b, c) =
  (a, b, handle_let_in_exp bind c)

let rec handle_qtf_in_fml fml =
  { fml with data = handle_qtf_in_prim_fml fml.loc fml.data }
and handle_qtf_in_exp exp =
  { exp with data = handle_qtf_in_prim_exp exp.data }
and handle_qtf_in_iexp iexp =
  { iexp with data = handle_qtf_in_prim_iexp iexp.data }
and handle_qtf_in_prim_fml loc p =
  match p with
    | Let (a, b) -> let_ a (List.map handle_qtf_in_fml b)
    | Block a -> block (List.map handle_qtf_in_fml a)
    | FBuiltin (s, l) -> fbuiltin s (List.map handle_qtf_in_exp l)
    | Qual (q, p) -> qual q (handle_qtf_in_exp p)
    | RComp (p1, c, p2) ->
        rcomp (handle_qtf_in_exp p1) c (handle_qtf_in_exp p2)
    | IComp (i1, c, i2) ->
        icomp (handle_qtf_in_iexp i1) c (handle_qtf_in_iexp i2)
    | LUn (op, p) -> lunary op (handle_qtf_in_fml p)
    | LBin (p1, op, p2) ->
        lbinary (handle_qtf_in_fml  p1) op (handle_qtf_in_fml p2)
    | QAEN(a, b, c) ->
        let b = (CCList.flat_map handle_qtf_in_sim_bindings b) in
        let rec flatten = function
            [] -> assert false
          | [h] -> qaen a [h] (List.map handle_qtf_in_fml c)
          | h::t -> qaen a [h] [{ data = flatten t; loc = loc }] in
        flatten b
    | QLO (a, b, c) ->
        let b = (List.map handle_qtf_in_bindings b) in
        qlo a b (List.map handle_qtf_in_fml c)
    | FIte (p1, p2, p3) ->
        fite (handle_qtf_in_fml p1) (handle_qtf_in_fml p2)
          (handle_qtf_in_fml p3)
    | True
    | False
      -> p
and handle_qtf_in_prim_exp expr =
  match expr with
    | RUn (c, p) -> runary c (handle_qtf_in_exp  p)
    | RBin (p1, c, p2) ->
        rbinary (handle_qtf_in_exp  p1) c (handle_qtf_in_exp  p2)
    | RIte (f, p1, p2) -> rite (handle_qtf_in_fml f)
                            (handle_qtf_in_exp p1) (handle_qtf_in_exp p2)
    | BoxJoin (e, l) ->
        boxjoin (handle_qtf_in_exp e) (List.map handle_qtf_in_exp l)
    | Compr (sb, b) ->
        compr sb (List.map (handle_qtf_in_fml ) b)
    | Prime p -> prime (handle_qtf_in_exp  p)
    | Ident _
    | None_
    | Univ
    | Iden
      -> expr
and handle_qtf_in_prim_iexp p =
  match p with
    | Num _ -> p
    | Card e -> card (handle_qtf_in_exp e)
    | IUn (o, i) -> iunary o (handle_qtf_in_iexp i)
    | IBin (i1, o, i2) ->
        ibinary (handle_qtf_in_iexp i1) o (handle_qtf_in_iexp  i2)
and handle_qtf_in_bindings (a, b) =
  (a, handle_qtf_in_exp  b)
and handle_qtf_in_sim_bindings (a, b, c) =
  match a with
      true -> [(a, b, handle_qtf_in_exp c)]
    | false -> List.map (fun x -> (a, [x], handle_qtf_in_exp c)) b

let rec handle_boxjoin_in_fml fml =
  { fml with data = handle_boxjoin_in_prim_fml fml.data }
and handle_boxjoin_in_exp exp =
  { exp with data = handle_boxjoin_in_prim_exp exp.data }
and handle_boxjoin_in_iexp iexp =
  { iexp with data = handle_boxjoin_in_prim_iexp iexp.data }
and handle_boxjoin_in_prim_fml p =
  match p with
    | Let (a, b) -> let_ a (List.map handle_boxjoin_in_fml b)
    | Block a -> block (List.map handle_boxjoin_in_fml a)
    | FBuiltin (s, l) -> fbuiltin s (List.map handle_boxjoin_in_exp l)
    | Qual (q, p) -> qual q (handle_boxjoin_in_exp p)
    | RComp (p1, c, p2) ->
        rcomp (handle_boxjoin_in_exp p1) c (handle_boxjoin_in_exp p2)
    | IComp (i1, c, i2) ->
        icomp (handle_boxjoin_in_iexp i1) c (handle_boxjoin_in_iexp i2)
    | LUn (op, p) -> lunary op (handle_boxjoin_in_fml p)
    | LBin (p1, op, p2) ->
        lbinary (handle_boxjoin_in_fml  p1) op (handle_boxjoin_in_fml p2)
    | QAEN(a, b, c) ->
        let b = (List.map handle_boxjoin_in_sim_bindings b) in
        qaen a b (List.map handle_boxjoin_in_fml c)
    | QLO (a, b, c) ->
        let b = (List.map handle_boxjoin_in_bindings b) in
        qlo a b (List.map handle_boxjoin_in_fml c)
    | FIte (p1, p2, p3) ->
        fite (handle_boxjoin_in_fml p1) (handle_boxjoin_in_fml p2)
          (handle_boxjoin_in_fml p3)
    | True
    | False
      -> p
and handle_boxjoin_in_prim_exp expr =
  match expr with
    | RUn (c, p) -> runary c (handle_boxjoin_in_exp  p)
    | RBin (p1, c, p2) ->
        rbinary (handle_boxjoin_in_exp  p1) c (handle_boxjoin_in_exp  p2)
    | RIte (f, p1, p2) -> rite (handle_boxjoin_in_fml f)
                            (handle_boxjoin_in_exp p1) (handle_boxjoin_in_exp p2)
    | BoxJoin (e, l) ->
        let rec dotify exp = function
            [] -> exp.data
          | h::t -> rbinary h join exp in
        handle_boxjoin_in_prim_exp (dotify e l)
    | Compr (sb, b) ->
        compr sb (List.map (handle_boxjoin_in_fml ) b)
    | Prime p -> prime (handle_boxjoin_in_exp  p)
    | Ident _
    | None_
    | Univ
    | Iden
      -> expr
and handle_boxjoin_in_prim_iexp p =
  match p with
    | Num _ -> p
    | Card e -> card (handle_boxjoin_in_exp e)
    | IUn (o, i) -> iunary o (handle_boxjoin_in_iexp i)
    | IBin (i1, o, i2) ->
        ibinary (handle_boxjoin_in_iexp i1) o (handle_boxjoin_in_iexp  i2)
and handle_boxjoin_in_bindings (a, b) =
  (a, handle_boxjoin_in_exp  b)
and handle_boxjoin_in_sim_bindings (a, b, c) =
  (a, b, handle_boxjoin_in_exp c)

let whole elo_pb =
  Msg.debug (fun m -> m "Elo AST =@;%a" (Elo.pp) elo_pb);

  let substitute m (Sat b) = sat (List.map m b) in

  (* let substitution *)
  Msg.debug (fun m -> m "Elo_cleanup: let substitution");
  let elo_goals_no_let =
    List.map (substitute (handle_let_in_fml [])) elo_pb.goals in

  (* split sim_bindings *)
  Msg.debug (fun m -> m "Elo_cleanup: split sim_bindings");
  let elo_goals_split_qtf_vars =
    List.map (substitute handle_qtf_in_fml) elo_goals_no_let in

  Msg.debug (fun m -> m "Elo_cleanup: transform box joins");
  let elo_goals_no_boxjoin =
    List.map (substitute handle_boxjoin_in_fml) elo_goals_split_qtf_vars in

  (* return *)
  Msg.debug (fun m -> m "Elo_cleanup: step completed");
  make elo_pb.file elo_pb.domain elo_goals_no_boxjoin

let transfo = Transfo.make "elo_cleanup" whole

