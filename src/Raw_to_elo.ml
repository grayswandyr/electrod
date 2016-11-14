open Containers
open Raw
    

(*******************************************************************************
 *  Domain computation
 *******************************************************************************)

let split_indexed_id infile id =
  let name, loc = Raw_ident.(basename id, location id) in
  match String.Split.right ~by:"$" name with
    | None -> assert false      (* comes from the lexer so cannot be None*)
    | Some (left, right) ->
        let rightnum =
          try int_of_string right
          with Failure _ ->
            Msg.Fatal.wrong_suffix (fun args -> args infile id)
        in
        (left, rightnum)

(* check whether [atoms] contains duplicate atoms, warn about them and return the de-duplicated list *)
let check_duplicate_atoms infile atoms =
  (* sort and remove duplicates *)
  let dedup = List.sort_uniq ~cmp:Atom.compare atoms in
  (* check whether we lost elements by doing this*)
  if List.length atoms > List.length dedup then
    Msg.Warn.univ_duplicate_atoms
      (fun args -> args infile (List.sort Atom.compare atoms) dedup);
  dedup

let interval_to_atoms infile (first, last) =
  let firstbasename, firstnum = split_indexed_id infile first in
  let lastbasename, lastnum = split_indexed_id infile last in
  if String.compare firstbasename lastbasename <> 0 then
    Msg.Fatal.different_prefixes (fun args -> args infile first last)
  else if firstnum > lastnum then
    Msg.Fatal.not_an_interval (fun args -> args infile first last)
  else
    let open List in
    firstnum --  lastnum
    |> map (fun num ->
          Atom.atom @@ Printf.sprintf "%s$%d" firstbasename num)


let compute_univ infile raw_univ =
  let open List in
  let atoms =
    flat_map
      (function | UIntvl intvl -> interval_to_atoms infile intvl
                | UPlain id -> [ Atom.of_raw_ident id ]) raw_univ
  in
  let dedup = check_duplicate_atoms infile atoms in
  let bound = List.map Tuple.tuple1 dedup |> Bound.of_tuples in
  Relation.(const Name.univ @@ Scope.exact bound)
       
(* returns a list of tuples (possibly 1-tuples corresponding to plain atoms) *)
let compute_tuples infile domain = function
  (* a list of  1-tuples (coming from indexed id's) *)
  | EIntvl intvl ->
      (* Msg.debug (fun m -> m "Raw_to_elo.compute_tuples:EIntvl"); *)
      let atoms = interval_to_atoms infile intvl in
      let absent =   (* compute 1-tuples/atoms absent from univ, if there are *)
        List.flat_map
          (fun t ->
             if not @@ Bound.mem (Tuple.tuple1 t) @@ Domain.univ_atoms domain
             then [t] else []) atoms in
      (if absent <> [] then
         Msg.Fatal.undeclared_atoms
         @@ fun args -> args
                          infile 
                          (Location.span
                           @@ Pair.map_same Raw_ident.location intvl)
                          absent);      
      let dedup = check_duplicate_atoms infile atoms in
      List.map Tuple.tuple1 dedup
  | ETuple [] -> assert false   (* grammatically impossible *)
  (* a single n-ary tuple *)
  | ETuple ids ->
      (* Msg.debug (fun m -> m "Raw_to_elo.compute_tuples:ETuple"); *)
      let atoms = List.map (fun id -> Raw_ident.basename id |> Atom.atom) ids in
      (* to check if all atoms in the tuple are in univ, we do as if every atom
         was a 1-tuple and then check whether this 1-tuple is indeed in univ *)
      let absent =   (* compute 1-tuples/atoms absent from univ, if there are *)
        List.flat_map
          (fun t ->
             if not @@ Bound.mem (Tuple.tuple1 t) @@ Domain.univ_atoms domain
             then [t] else []) atoms in
      (if absent <> [] then
         Msg.Fatal.undeclared_atoms
         @@ fun args ->
         args infile
           (Location.span
            @@ Pair.map_same Raw_ident.location List.(hd ids, hd @@ last 1 ids))
           absent); 
      [Tuple.of_list1 atoms]


(* [`Inf] and [`Sup] tell whether we are computing a lower of upper bound:
   this is important as a bound may be defined out of other ones, so we
   should know whether we need the lower or upper bound of the relations
   referred to. The variants are in an [option] which is set to [None] if
   the scope is exact (in which case, the variants are of no use); [Some]
   otherwise.

   We also pass the [id] of the concerned relation (useful for error message). *)
let compute_bound infile domain (which : [ `Inf | `Sup] option) id raw_bound =
  let open Relation in
  let open Scope in
  let rec walk = function
    | BUniv ->
        (* Msg.debug (fun m -> m "Raw_to_elo.compute_bound:BUniv"); *)
        Domain.univ_atoms domain
    | BRef ref_id ->
        (* Msg.debug (fun m -> m "Raw_to_elo.compute_bound:BRef"); *)
        begin
          match Domain.get (Name.of_raw_ident ref_id) domain with
            | None -> Msg.Fatal.undeclared_id (fun args -> args infile ref_id)
            | Some rel ->
                match rel with
                  | Const { scope = Exact b } when Bound.arity b = 1 -> b
                  | Const { scope = Inexact (inf, sup) }
                    when Bound.arity sup = 1 ->
                      (match which with
                        | Some `Inf -> inf
                        | Some `Sup -> sup
                        | None ->
                            Msg.Fatal.inexact_ref_used_in_exact_scope
                            @@ fun args -> args infile id ref_id
                      )
                  | Const _ | Var _ ->
                      Msg.Fatal.should_denote_a_constant_set
                      @@ fun args -> args infile ref_id
        end
    | BProd (rb1, rb2) ->
        (* Msg.debug (fun m -> m "Raw_to_elo.compute_bound:BProd"); *)
        let b1 = walk rb1 in
        let b2 = walk rb2 in
        Bound.product b1 b2
    | BUnion (rb1, rb2) ->
        (* Msg.debug (fun m -> m "Raw_to_elo.compute_bound:BUnion"); *)
        let b1 = walk rb1 in
        let b2 = walk rb2 in
        if Bound.(arity b1 = arity b2) then
          Bound.union b1 b2
        else
          Msg.Fatal.incompatible_arities @@ fun args -> args infile id
    | BElts elts ->
        (* Msg.debug (fun m -> m "Raw_to_elo.compute_bound:BElts"); *)
        let tuples = List.flat_map (compute_tuples infile domain) elts in 
        match tuples with
          | [] -> Bound.empty
          | t::ts -> 
              let ar = Tuple.arity t in
              if List.exists (fun t2 -> Tuple.arity t2 <> ar) ts then
                Msg.Fatal.incompatible_arities (fun args -> args infile id);
              let bnd = Bound.of_tuples tuples in
              if Bound.size bnd <> List.length tuples then
                Msg.Warn.duplicate_elements
                  (fun args -> args infile id which bnd);
              bnd
  in
  walk raw_bound 
  

let compute_scope infile domain id = function
  | SExact raw_b ->
      (* Msg.debug (fun m -> m "Raw_to_elo.compute_scope:SExact"); *)
      Scope.exact @@ compute_bound infile domain None id raw_b 

  | SInexact (raw_inf, raw_sup) ->
      (* Msg.debug (fun m -> m "Raw_to_elo.compute_scope:SInexact"); *)
      let inf = compute_bound infile domain (Some `Inf) id raw_inf in
      let sup = compute_bound infile domain (Some `Sup) id raw_sup in
      let ar_inf = Bound.arity inf in
      let ar_sup = Bound.arity sup in
      if ar_inf <> ar_sup && not (Bound.is_empty inf) then
        Msg.Fatal.incompatible_arities (fun args -> args infile id);
      if not @@ Bound.subset inf sup then
        Msg.Fatal.inf_not_in_sup (fun args -> args infile id inf sup);
      if Bound.is_empty sup then
        Msg.Warn.empty_scope_declared (fun args -> args infile id);
      if Bound.equal inf sup then
        Scope.exact sup
      else
        Scope.inexact inf sup
          

let check_name infile id domain = 
  let name = Name.of_raw_ident id in
  (if Domain.mem name domain then
     Msg.Fatal.rel_name_already_used @@ fun args -> args infile id)
                       
let compute_decl infile domain = function
  | DVar (id, init, fby) ->
      (* Msg.debug (fun m -> m "Raw_to_elo.compute_decl:DVar"); *)
      check_name infile id domain;
      let init = compute_scope infile domain id init in
      Relation.var (Name.of_raw_ident id)
        init
        (CCOpt.map (compute_scope infile domain id) fby)
      
  | DConst (id, raw_scope) ->
      (* Msg.debug (fun m -> m "Raw_to_elo.compute_decl:DConst"); *)
      check_name infile id domain;
      let scope = compute_scope infile domain id raw_scope in
      Relation.const (Name.of_raw_ident id) scope


let compute_domain (pb : (Raw_ident.t, Raw_ident.t) Raw.raw_problem) =
  let univ = compute_univ pb.file pb.raw_univ in
  let init = Domain.add Name.univ univ Domain.empty in
  (* updating the domain, given a previous domain and a raw_decl *)
  let update dom decl =
    let name = Name.of_raw_ident @@ Raw.decl_id decl in
    let rel = compute_decl pb.file dom decl in
    let newdom = Domain.add name rel dom in
    (* Msg.debug *)
    (*   (fun m -> m "Raw_to_elo.compute_domain:update add %a ⇒ %a" *)
    (*               Name.pp name (Fmtc.hbox @@ Domain.pp) newdom); *)
    newdom
  in
  List.fold_left update init pb.raw_decls


(*******************************************************************************
 *  Walking along raw goals to get variables and relation names out of raw_idents
 *******************************************************************************)

let refine_identifiers raw_pb =
  let open Raw_goal in
  let rec walk_fml ctx fml =
    let ctx2, f = walk_prim_fml ctx fml.data in
    (ctx2, { fml with data = f })

  and walk_prim_fml ctx = function
    | QLO (q, bindings, blk) ->
       let ctx2, bindings2 = walk_bindings ctx bindings in
       let _, blk2 = walk_block ctx2 blk in
       (ctx, qlo q bindings2 blk2)
    | QAEN (q, sim_bindings, blk) ->
       let ctx2, sim_bindings2 = walk_sim_bindings ctx sim_bindings in
       let _, blk2 = walk_block ctx2 blk in
       (ctx, qaen q sim_bindings2 blk2) 
    | True -> (ctx, true_)
    | False -> (ctx, false_)
    | Block b -> (ctx, Pair.map_snd block (walk_block ctx b))
    | LUn (op, fml) -> (ctx, Pair.map_snd (lunary op) (walk_fml ctx fml))
    | LBin (f1, op, f2) ->
       (ctx, lbinary (snd @@ walk_fml ctx f1) op (snd @@ walk_fml ctx f2))
    | FBuiltin (str, args) ->
       (ctx, fbuiltin str @@ List.map (walk_exp ctx) args)
    | Qual (q, r) -> (ctx, qual rsome @@ walk_exp ctx r)
    | Comp (e1, op, e2) -> (ctx, comp (walk_exp ctx e1) op (walk_exp ctx e2))
    | FIte (c, t, e) ->
       (ctx, fite (snd @@ walk_fml ctx c) (snd @@ walk_fml ctx t) (snd @@ walk_fml ctx e))
    | Let (bindings, blk) -> 
       let ctx2, bindings2 = walk_bindings ctx bindings in
       let _, blk2 = walk_block ctx2 blk in
       (ctx, let_ bindings2 blk2)

  and walk_bindings ctx = function
    | [] -> (ctx, [])
    | b :: bs ->
       let ctx2, b2 = walk_binding ctx b in
       let ctx3, bs2 = walk_bindings ctx2 bs in
       (ctx3, b2 :: bs2)

  and walk_binding ctx (v, exp) =
    let exp2 = walk_exp ctx exp in
    let var = `Var (Var.fresh (Raw_ident.basename v)) in
    ((v, var) :: ctx, (var, exp2))

  and walk_sim_bindings ctx = function
    | [] -> (ctx, [])
    | sb :: sbs ->
       let ctx2, sb2 = walk_sim_binding ctx sb in
       let ctx3, sbs2 = walk_sim_bindings ctx2 sbs in 
       (ctx3, sb2 :: sbs2)

  and walk_sim_binding ctx (disj, vs, exp) = 
    (if disj && List.length vs = 1 then
        Msg.Warn.disj_with_only_one_variable @@ fun args -> args raw_pb.file (List.hd vs));
    let exp2 = walk_exp ctx exp in
    let vars = List.map (fun v -> `Var (Var.fresh (Raw_ident.basename v))) vs in
    (List.(combine vs vars |> rev) @ ctx, (disj, vars, exp2))      

  and walk_block ctx blk =
    (ctx, List.map (fun fml -> snd @@ walk_fml ctx fml) blk)

  and walk_exp ctx exp =
    { exp with data = walk_prim_exp ctx exp.data }

  and walk_prim_exp ctx = function
    | Ident id ->
       (try
          ident @@ CCList.Assoc.get_exn ~eq:Raw_ident.eq_name ctx id
        with Not_found ->
          Msg.Fatal.undeclared_id @@ fun args -> args raw_pb.file id)
    | RBuiltin (str, args) -> rbuiltin str @@ List.map (walk_exp ctx) args
    | IBuiltin (str, args) -> ibuiltin str @@ List.map (walk_exp ctx) args
    | Num n -> num n
    | None_ -> none
    | Univ -> univ
    | Iden -> iden
    | Int -> int
    | RUn (op, e) -> runary op @@ walk_exp ctx e
    | RBin (e1, op, e2) -> rbinary (walk_exp ctx e1) op (walk_exp ctx e2)
    | RIte (c, t, e) -> rite (snd @@ walk_fml ctx c) (walk_exp ctx t) (walk_exp ctx e)
    | BoxJoin (e, args) -> boxjoin (walk_exp ctx e) @@ List.map (walk_exp ctx) args
    | Prime e -> prime (walk_exp ctx e) 
    | Compr (sim_binding, blk) ->
       let ctx2, sim_binding2 = walk_sim_binding ctx sim_binding in
       let _, blk2 = walk_block ctx2 blk in
       compr sim_binding2 blk2

  in
  (* initial context is made of relation names declared in the domain (+ univ) *)
  let init_ctx =
    List.map
      (fun decl ->
        Pair.dup_map (fun id -> `Name (Name.of_raw_ident id))
        @@ Raw.decl_id decl)
      raw_pb.raw_decls
    @ [ (Raw_ident.ident "univ" Lexing.dummy_pos Lexing.dummy_pos,
         `Name Name.univ) ]
  in
  let walk_goal (Sat blk) : Elo.goal = 
    sat @@ snd @@ walk_block init_ctx blk
  in
  List.map walk_goal raw_pb.raw_goals


(*******************************************************************************
 *  Declaration of the whole transformation
 *******************************************************************************)

let whole raw_pb =
  let domain = compute_domain raw_pb in
  let goals = refine_identifiers raw_pb in
  Elo.make raw_pb.file domain goals

let transfo = Transfo.make "raw_to_elo" whole (* temporary *)
