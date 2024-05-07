(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2024 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Functor that provides a {!Elo_to_LTL_intf.S} converter given an
    implementation of LTL *)

open Containers
open Exp_bounds
module E = Elo
module TS = Tuple_set

type stack = Tuple.t list

let pp_subst out subst =
  Fmtc.(brackets @@ list @@ parens @@ pair int Tuple.pp)
    out
    (List.mapi (fun i tuple -> (i, tuple)) subst)

let all_different ~eq xs =
  let rec walk acc = function
    | [] -> true
    | [ hd ] -> not @@ List.mem ~eq hd acc
    | hd :: tl -> (not @@ List.mem ~eq hd acc) && walk (hd :: acc) tl
  in
  walk [] xs

module Make (Ltl : Solver.LTL) = struct
  open Ltl
  open Ltl.Infix

  type atomic = Ltl.Atomic.t
  type ltl = Ltl.t
  type goal = E.t

  (***************************************************************** 
   * Semantic function
   ****************************************************************)

  (* FIRST: some functions used for the semantics of a transitive closure. *)

  (* given a 2-tuple set ts, this function computes the domain and the
     co-domain of ts, i.e., the set (sequence) of atoms that are the
     first elements of a 2-tuple in ts, and the set (sequence) of atoms
     thare are the second elements of a 2-tuple in ts *)
  let compute_domain_codomain ts =
    let ar = TS.inferred_arity ts in
    assert (ar = 2);
    let module S = Iter in
    let s = TS.to_iter ts in
    let split_seq (s1_acc, s2_acc) tup =
      (S.cons (Tuple.ith 0 tup) s1_acc, S.cons (Tuple.ith 1 tup) s2_acc)
    in
    S.fold split_seq (S.empty, S.empty) s
    |> Fun.tap @@ fun res ->
       Msg.debug (fun m ->
           m "compute_domain_codomain(%a) --> (ar = %d)@ = %a" TS.pp ts ar
             (Fmtc.parens
             @@ Pair.pp
                  ~pp_sep:Fmtc.(const string ", ")
                  (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp)
                  (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp))
             res)

  (* given a 2-tuple set, this function computes the maximum length of
     a path (x1, ... xn) such that each 2-tuple (xi, xi+1) is in the
     tuple set.  Used to compute the number of iterations needed for
     transitive closure term. *)
  let compute_tc_length ts =
    let tsarity = TS.inferred_arity ts in
    Msg.debug (fun m -> m "compute_tc_length: arity of relation : %d\n" tsarity);
    assert (tsarity = 2 || tsarity = 0);
    if tsarity = 0 then 0
    else
      let module S = Iter in
      let dom, cod = compute_domain_codomain ts in
      let core_ats = S.inter ~eq:Atom.equal ~hash:Atom.hash dom cod in
      Msg.debug (fun m ->
          m "compute_tc_length: inter %a %a = %a\n"
            (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp)
            dom
            (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp)
            cod
            (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp)
            core_ats);
      let core_length = S.length core_ats in
      (* is it possible that x1 is not in the core (intersection of the
         domain and the codomain) ? *)
      let first_elt_in_core =
        S.subset ~eq:Atom.equal ~hash:Atom.hash dom core_ats
      in
      Msg.debug (fun m ->
          m "compute_tc_length: first_elt_in_core = %B\n" first_elt_in_core);
      (* is it possible that xn is not in the core (intersection of the
         domain and the codomain) ? *)
      let last_elt_in_core =
        S.subset ~eq:Atom.equal ~hash:Atom.hash cod core_ats
      in
      Msg.debug (fun m ->
          m "compute_tc_length: last_elt_in_core = %B\n" last_elt_in_core);
      (match (first_elt_in_core, last_elt_in_core) with
      | true, true -> core_length
      | false, false -> core_length + 2
      | _ -> core_length + 1)
      |> Fun.tap (fun res ->
             Msg.debug (fun m -> m "compute_tc_length --> length = %d" res))

  (* computes the transitive closure of the term acc_term by k iterative
     squares (t+t.t)+(t+t.t)(t+t.t) + ... *)

  let rec iter_squares (acc_term : E.exp) k =
    match k with
    | 0 -> E.none
    | 1 -> acc_term
    | _ ->
        let ar = E.arity acc_term in
        let new_exp =
          E.(rbinary ~ar acc_term union @@ rbinary ~ar acc_term join acc_term)
        in
        iter_squares new_exp (max (k lsr 1) ((k + 1) lsr 1))

  (* computes the transitive closure of the term t by k joins
     (alternative to iter_squares) t + t.t + t.t.t + ... *)

  (* let iter_tc (t : E.exp) k =
     if k = 0 then E.none
     else
     let ar = E.arity t in
     let t_to_the_k = ref t in
     let tc = ref t in
     for _ = 2 to k do
     t_to_the_k := E.(rbinary ~ar !t_to_the_k join t);
     tc := E.(rbinary ~ar !tc union !t_to_the_k);
     done;
     !tc *)

  (* computes the transitive closure of the term t by t.(iden + t).(iden.t^2).(iden+(t^2)^2)... *)

  (* let ioannidis_tc (t : E.exp) k =
     let ar = E.arity t in
     let prev_t = ref t in
     let term = ref E.(rbinary ~ar t join (rbinary ~ar iden union t)) in
     let i = ref 1 in (* 0 for (iden + t) *)
     let max_pow = ref 2 in (* 1 for t^1 + 1 for first 't.'*)
     while !max_pow <= k do
     prev_t := E.(rbinary ~ar !prev_t join !prev_t);
     term := E.(rbinary ~ar !term join (rbinary ~ar iden union !prev_t));
     i := 2 * !i;
     max_pow := !max_pow + !i
     done;
     !term *)

  (* utility function for build_Join *)
  let eligible_pairs ((tuple, r_sup, s_sup) : Tuple.t * TS.t * TS.t) :
      (Tuple.t * Tuple.t) Iter.t =
    let open Iter in
    let r_sup_seq = TS.to_iter r_sup in
    (* filtering candidates (sharing a prefix or suffix with tuple) may be good *)
    let s_sup_seq = TS.to_iter s_sup in
    fold
      (fun pairs x_r ->
        find
          (* find the *at most one* (for fixed tuple and x_r) x_s s.t. tuple = x_r . x_s *)
            (fun x_s ->
            if Tuple.is_in_join tuple x_r x_s then Some (x_r, x_s) else None)
          s_sup_seq
        |> function
        | Some pair -> cons pair pairs
        | None -> pairs)
      empty r_sup_seq

  (* Produces the integer term corresponding to [SUM_(t \in on) f(t)]  *)
  let summation ~bw ~(on : TS.t) (f : Tuple.t -> term) : term =
    on |> TS.to_iter |> Iter.fold (fun t1 t2 -> plus t1 (f t2)) (num bw 0)

  module TupleHashtbl = CCHashtbl.Make (Tuple)

  let int_tuples_as_ints int_set : int TupleHashtbl.t =
    (* converts the set of ints into a hashtable where the key is a 1-tuple containing an int and the data is the corresponding int*)
    int_set |> Tuple_set.to_iter
    |> Iter.map (fun t ->
           (t, t |> Tuple.to_list |> List.hd |> Atom.to_string |> int_of_string))
    |> TupleHashtbl.of_iter

  let convert_triple bitwidth tuple =
    let int_list =
      tuple |> Tuple.to_1tuples
      |> List.map Fun.(Tuple.to_string %> int_of_string %> num bitwidth)
    in
    match int_list with [ a; b; c ] -> (a, b, c) | _ -> assert false

  (* converts a 3-column relation (for shifts) into a list Ltl-term triples with the correct bitwidth *)
  let convert_relation_to_int_triples bitwidth triples =
    match triples with
    | None -> []
    | Some ts -> ts |> Tuple_set.to_list |> List.map (convert_triple bitwidth)

  let rec create_shift_formula l r triples =
    match triples with
    | [] -> failwith "create_shift_formula: the shift relation is empty"
    | [ (_, _, sh) ] -> sh
    | (a, b, sh) :: tl ->
        ifthenelse_arith
          (and_ (comp eq l a) @@ lazy (comp eq r b))
          sh
          (create_shift_formula l r tl)

  class environment (elo : Elo.t) =
    (* Atomic.make is a cached function for its last two arguments (out of 3), so we compute it for its first argument to avoid unnecessary recomputations *)
    let computed_bitwidth = Domain.bitwidth elo.domain in
    let make_atom_aux = Atomic.make elo.domain in
    let int_set = Domain.ints elo.domain in
    let int_of_tuple =
      let tbl = int_tuples_as_ints int_set in
      fun t -> TupleHashtbl.find_opt tbl t
    in
    object (_ : 'self)
      val bounds_exp_aux = Exp_bounds.make_bounds_exp elo.Elo.domain
      val bitwidth = computed_bitwidth

      val shl_term_triples =
        convert_relation_to_int_triples computed_bitwidth
          (Domain.shl elo.domain)

      val shr_term_triples =
        convert_relation_to_int_triples computed_bitwidth
          (Domain.shr elo.domain)

      val sha_term_triples =
        convert_relation_to_int_triples computed_bitwidth
          (Domain.sha elo.domain)

      method must_may_sup (subst : stack) (exp : E.exp) =
        bounds_exp_aux (exp, subst)

      method relation_arity name =
        match Domain.get name elo.domain with
        | None -> assert false
        | Some rel -> Relation.arity rel

      method make_atom (name : Name.t) (t : Tuple.t) =
        assert (Domain.mem name elo.domain);
        Ltl.atomic @@ make_atom_aux name t

      method is_const (name : Name.t) =
        assert (Domain.mem name elo.Elo.domain);
        Domain.get_exn name elo.Elo.domain |> Relation.is_const

      method bitwidth = bitwidth
      method int_set = int_set
      method int_of_tuple (t : Tuple.t) = int_of_tuple t
      method shl_list = shl_term_triples
      method shr_list = shr_term_triples
      method sha_list = sha_term_triples
    end

  class ['subst] converter (env : environment) =
    object (self : 'self)
      constraint 'subst = stack

      (* a stack *)
      inherit ['self] Elo_recursor.recursor
      method build_Add (_ : stack) (a : term) (b : term) : term = plus a b
      method build_All (_ : stack) = E.all
      method build_And (_ : stack) (a : ltl) (b : ltl) : ltl = and_ a (lazy b)
      method build_Block (_ : stack) = conj

      (* re-defining this method to avoid going down in the block as a
         substitution must be made first *)
      method! visit_Compr env _visitors_c0 _visitors_c1 =
        let _visitors_r0 =
          self#visit_list
            (fun env (_visitors_c0, _visitors_c1, _visitors_c2) ->
              let _visitors_r0 =
                (fun _visitors_this -> _visitors_this) _visitors_c0
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_c1
              in
              let _visitors_r2 = self#visit_'exp env _visitors_c2 in
              (_visitors_r0, _visitors_r1, _visitors_r2))
            env _visitors_c0
        in
        let _visitors_r1 = [ true_ ] in
        self#build_Compr env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

      method private allocate_sbs_to_tuples (ranges : E.exp list)
          (tuple : Tuple.t) : Tuple.t list =
        let rec walk ranges atoms =
          match ranges with
          | [] -> []
          | hd :: tl ->
              let xs, ys = List.take_drop (E.arity hd) atoms in
              Tuple.of_list1 xs :: walk tl ys
        in
        walk ranges @@ Tuple.to_list tuple

      (* check if the disj's in the comprehension sim_bindings are respected *)
      method private check_compr_disj (sbs : (bool * int * E.exp) list)
          (split_tuples : Tuple.t list) : bool =
        let rec walk sbs tuples =
          match sbs with
          | [] -> true
          | (true, nbvars, _) :: tl ->
              let xs, ys = List.take_drop nbvars tuples in
              let alldiff = all_different ~eq:Tuple.equal xs in
              Msg.debug (fun m ->
                  m "check_compr_disj (true, %d, _) tuples = %a alldiff = %B"
                    nbvars
                    Fmtc.(brackets @@ list ~sep:sp @@ Tuple.pp)
                    tuples alldiff);
              alldiff && walk tl ys
          | (false, nbvars, _) :: tl ->
              let ys = List.drop nbvars tuples in
              walk tl ys
        in
        walk sbs split_tuples

      (* shape: [{ sb1, sb2,... | b }]. Each [sb] is of shape [disj nbvar: e] .

         The first item implies that we have to fold over the [sb]'s to substitute
         previously-bound variables. In the following function, we perform these
         substitutions and then compute separately the semantics of every binding,
         before computing the whole resulting formula.
      *)
      method build_Compr (subst : stack) (sbs : (bool * int * E.exp) list)
          (body : E.fml list) __sbs' __body' tuple =
        let compr_ar =
          List.fold_left (fun acc (_, n, r) -> acc + (n * E.arity r)) 0 sbs
        in
        let depth = List.length subst in
        if Tuple.arity tuple <> compr_ar then
          Msg.err (fun m ->
              m
                "%s.build_Compr [[{%a@ |@ %a}]]_%a(%a): tuple arity (%d) \
                 incompatible with expression arity (%d)"
                __MODULE__ (E.pp_sim_bindings depth) sbs (E.pp_block depth) body
                pp_subst subst Tuple.pp tuple (Tuple.arity tuple) compr_ar);
        (* the tuple is (in principle) of arity equal to the sum of arities of ranges of bound variables. To build the corresponding substitutions, we must first split this tuple into as many tuples as variables, each one with the adequate arity *)
        let ranges =
          List.flat_map
            (fun (_, nbvars, range) -> List.repeat nbvars [ range ])
            sbs
        in
        let split_tuples = self#allocate_sbs_to_tuples ranges tuple in
        if self#check_compr_disj sbs split_tuples then
          (* semantics of [b] is [[ b [tuples / variables] ]] *)
          let b' =
            self#visit_fml (List.rev split_tuples @ subst) @@ E.block body
          in
          (* every single sim_binding contains possibly many variables and they may depend over previous bindings of the same comprehension. Because of the many variables, we use [fold_flat_map] which is like a fold returning a pair of an accumulator and a list, the latter undergoing flattening *)
          let _, ranges' =
            List.fold_flat_map
              (fun (acc_split_tuples, acc_subst) (_, nbvars, r) ->
                let boundvars, remaining =
                  List.take_drop nbvars acc_split_tuples
                in
                let r' = self#visit_exp acc_subst r (Tuple.concat boundvars) in
                (* copy range nbvars times *)
                let rs' = List.repeat nbvars [ r' ] in
                let new_subst = List.rev boundvars @ acc_subst in
                ((remaining, new_subst), rs'))
              (split_tuples, subst) sbs
          in
          conj (b' :: ranges')
        else false_

      method build_Diff (_ : stack) (_ : E.exp) (_ : E.exp) e' f'
          (tuple : Tuple.t) =
        e' tuple +&& lazy (not_ (f' tuple))

      method build_F (_ : stack) (a : ltl) : ltl = eventually a

      method build_FIte (_ : stack) _ _ _ (c : ltl) (t : ltl) (e : ltl) : ltl =
        ifthenelse c t e

      method build_False (_ : stack) : ltl = false_
      method build_G (_ : stack) (a : ltl) : ltl = always a
      method build_Gt (_ : stack) : tcomp = gt
      method build_Gte (_ : stack) : tcomp = gte
      method build_H (_ : stack) (a : ltl) : ltl = historically a
      method build_IBin (_ : stack) _ _ _ i1' op' i2' = op' i1' i2'

      method build_IComp (_ : stack) __e1 _ __e2 e1_r op e2_r =
        comp op e1_r e2_r

      method build_IEq (_ : stack) : tcomp = eq
      method build_INEq (_ : stack) : tcomp = neq
      method build_IUn (_ : stack) _ _ op' i' = op' i'

      method build_Iden (_ : stack) tuple =
        (* FIXME *)
        assert (Tuple.arity tuple = 2);
        if Atom.equal (Tuple.ith 0 tuple) (Tuple.ith 1 tuple) then true_
        else false_

      method build_Iff (_ : stack) (a : ltl) (b : ltl) : ltl = iff a b

      method build_Imp (_ : stack) (a : ltl) (b : ltl) : ltl =
        implies a (lazy b)

      method build_In subst r (__s : E.exp) r' s' =
        let { must; may; _ } = env#must_may_sup subst r in
        wedge ~range:(TS.to_iter must) (fun t -> lazy (s' t))
        +&& lazy
              (wedge ~range:(TS.to_iter may) (fun bs ->
                   lazy (r' bs @=> lazy (s' bs))))

      method build_Inter (_ : stack) _ _ e1 e2 tuple =
        e1 tuple +&& lazy (e2 tuple)

      method build_Join subst r s r' s' tuple =
        let sup_r = (env#must_may_sup subst r).sup in
        let sup_s = (env#must_may_sup subst s).sup in
        let pairs = eligible_pairs (tuple, sup_r, sup_s) in
        vee ~range:pairs (fun (bs, cs) -> lazy (r' bs +&& lazy (s' cs)))

      method build_LBin (_ : stack) _ _ _ f1' op' f2' = op' f1' f2'

      method build_LProj (_ : stack) _ _ s' r' tuple =
        (s' @@ Tuple.(of_list1 [ ith 0 tuple ])) +&& lazy (r' tuple)

      method build_LUn (_ : stack) _ _ op' f' = op' f'
      method build_Lt (_ : stack) : tcomp = lt
      method build_Lte (_ : stack) : tcomp = lte

      method build_Name (subst : stack) rel _ tuple =
        let { must; may; _ } =
          env#must_may_sup subst @@ E.name ~ar:(env#relation_arity rel) rel
        in
        if TS.mem tuple must then true_
        else if TS.mem tuple may then env#make_atom rel tuple
        else false_

      method build_No (_ : stack) = E.no_
      method build_None_ (_ : stack) __tuple = false_
      method build_Not (_ : stack) (a : ltl) : ltl = not_ a

      method build_NotIn (subst : stack) r s r' s' =
        not_ @@ self#build_In subst r s r' s'

      method build_Num (_ : stack) n _ = num env#bitwidth n
      method build_Neg (_ : stack) (a : term) : term = neg a
      method build_O (_ : stack) (a : ltl) : ltl = once a
      method build_Or (_ : stack) (a : ltl) (b : ltl) : ltl = or_ a (lazy b)

      method build_Over (subst : stack) __r s r' s' tuple =
        let { must; may; _ } = env#must_may_sup subst s in
        let proj1 x = Tuple.(of_list1 [ ith 0 x ]) in
        let mustpart =
          wedge ~range:(TS.to_iter must) (fun t ->
              lazy
                (if Tuple.equal (proj1 t) (proj1 tuple) then false_ else true_))
        in
        (* [newmay] helps compute the last AND above: it removes duplicate tuples
           that may appear in this translation: *)
        let newmay =
          let mktup t =
            let _, from_snd_elt = Tuple.split t 1 in
            Tuple.(proj1 tuple @@@ from_snd_elt)
          in
          TS.map mktup may
        in
        let maypart =
          not_ @@ vee ~range:(TS.to_iter newmay) (fun t -> lazy (s' t))
        in
        s' tuple +|| lazy (mustpart +&& lazy (r' tuple +&& lazy maypart))

      method build_P (_ : stack) (a : ltl) : ltl = yesterday a
      method build_Prime (_ : stack) _ e' tuple = next @@ e' tuple

      method build_Prod (_ : stack) r s r' s' tuple =
        (* we need to split [tuple] so we need the arity of [r]. If the
           arity is [None] (for 'none'), then we must just return
           false. Otherwise the tuple is split. *)
        match (E.arity r, E.arity s) with
        | 0, _ | _, 0 -> false_
        | ar_r, _ ->
            let t1, t2 = Tuple.split tuple ar_r in
            r' t1 +&& lazy (s' t2)

      method! visit_Quant subst q ((disj, nbvars, range) as sb) block =
        let q' = self#visit_quant subst q in
        let sb' = (disj, nbvars, self#visit_'exp subst range) in
        let range' = [ true_ ] in
        self#build_Quant subst q sb block q' sb' range'

      method build_Quant subst quant (disj, nbvars, s) blk _ (_, _, s') _ =
        let tuples_of_sim_binding (dom : Tuple.t list) : Tuple.t list Iter.t =
          let open List in
          (* create as many copies as necessary (= nb of variables) of the domain *)
          init nbvars (fun __idx -> dom)
          (* take their cartesian product *)
          |> cartesian_product
          (* remove lines where there are tuples in common if [disj = true] *)
          |> (if disj then
              filter (fun l ->
                  let sorted = sort_uniq ~cmp:Tuple.compare l in
                  length l = length sorted)
             else Fun.id)
          |> to_iter
        in
        (* [pos_or_neg] tells whether the quantifier was a [no ...], in
           which case we consider the whole as [all ... | not ...]. [link]
           tells how to connect a premise and a test in the may part of the
           formula. *)
        let bigop, smallop, link, pos_or_neg =
          match quant with
          | E.All -> (wedge, and_, implies, Fun.id)
          | E.Some_ -> (vee, or_, and_, Fun.id)
          | E.No -> (wedge, and_, implies, not_)
        in
        let sem_of_substituted_blk tuples =
          lazy
            (pos_or_neg
            @@ (self#visit_fml @@ List.rev tuples @ subst)
            (* [[...]] *)
            @@ E.block blk)
        in
        let { must; may; _ } = env#must_may_sup subst s in
        let mustpart =
          bigop
            ~range:(tuples_of_sim_binding @@ TS.to_list must)
            (fun tuples -> sem_of_substituted_blk tuples)
        in
        let maypart =
          lazy
            (bigop
               ~range:(tuples_of_sim_binding @@ TS.to_list may)
               (fun tuples ->
                 (* if several variables were bound to the same range, then
                     we must apply the characteristic function thereof to
                     every candidate tuples for these variables; and then
                     take the conjunction. Note: if several variables range
                     in the same set, then we will apply the characteristic
                     function many times to the same tuples: as an
                     optimization, we keep --only in the computation of the
                     premise-- only unique tuples to avoid this superfluous
                     repetition. *)
                 let premise =
                   wedge
                     ~range:
                       List.(to_iter @@ sort_uniq ~cmp:Tuple.compare tuples)
                     (fun tuple -> lazy (s' tuple))
                 in
                 (* Msg.debug (fun m -> m "(build_Quant.premise) %a" Ltl.pp premise); *)
                 lazy (link premise @@ sem_of_substituted_blk tuples)))
        in
        smallop mustpart maypart

      method build_R (_ : stack) (a : ltl) (b : ltl) : ltl = releases a b

      method build_RBin (_ : stack) (a : E.exp) (_ : E.rbinop) (b : E.exp) a'
          op' b' tuple =
        op' a b a' b' tuple

      method build_RComp (_ : stack) f1 __op f2 f1' op' f2' = op' f1 f2 f1' f2'

      method build_REq subst r s r' s' =
        let r_bounds = env#must_may_sup subst r in
        let s_bounds = env#must_may_sup subst s in
        let inter = TS.inter r_bounds.may s_bounds.may in
        wedge ~range:(TS.to_iter r_bounds.must) (fun t -> lazy (s' t))
        +&& lazy
              (wedge ~range:(TS.to_iter s_bounds.must) (fun t -> lazy (r' t)))
        +&& lazy
              (wedge ~range:(TS.to_iter inter) (fun bs ->
                   lazy (r' bs @<=> s' bs)))
        +&& lazy
              (wedge
                 ~range:(TS.to_iter @@ TS.diff r_bounds.may inter)
                 (fun bs -> lazy (r' bs @=> lazy (s' bs))))
        +&& lazy
              (wedge
                 ~range:(TS.to_iter @@ TS.diff s_bounds.may inter)
                 (fun bs -> lazy (s' bs @=> lazy (r' bs))))

      method build_RIte (_ : stack) (__c : E.fml) (__t : E.exp) __e c' t' e'
          tuple =
        (c' @=> lazy (t' tuple)) +&& lazy (not_ c' @=> lazy (e' tuple))

      method build_AIte (_ : stack) (__c : E.fml) (__t : E.iexp) __e c' t' e' =
        ifthenelse_arith c' t' e'

      method build_RNEq (subst : stack) r s r' s' =
        not_ @@ self#build_REq subst r s r' s'

      method build_RProj (_ : stack) _ _ r' s' tuple =
        let lg = Tuple.arity tuple in
        (s' @@ Tuple.of_list1 [ Tuple.ith (lg - 1) tuple ]) +&& lazy (r' tuple)

      method build_RTClos subst r _ tuple =
        (* FIXME *)
        assert (Tuple.arity tuple = 2);
        self#visit_Iden subst tuple
        +|| lazy (self#visit_RUn subst E.tclos r tuple)

      method build_RUn (_ : stack) (_ : E.runop) (e : E.exp) op' e' = op' e e'
      method build_S (_ : stack) (a : ltl) (b : ltl) : ltl = since a b
      method build_Some_ (_ : stack) = E.some
      method build_Sub (_ : stack) (a : term) (b : term) : term = minus a b
      method build_T (_ : stack) (a : ltl) (b : ltl) : ltl = triggered a b

      method build_TClos subst r __r' tuple =
        assert (Tuple.arity tuple = 2);
        Msg.debug (fun m ->
            m "%s.build_TClos <-- %a" __MODULE__ E.(pp_exp (arity r)) r);
        let { sup; _ } = env#must_may_sup subst r in
        let k = compute_tc_length sup in
        Msg.debug (fun m -> m "TC bound: %d" k);
        (* let tc_naif = iter_tc r k in
           let fml_tc_naif = self#visit_exp subst tc_naif tuple in *)
        let tc_square = iter_squares r k in
        let fml_tc_square = self#visit_exp subst tc_square tuple in
        (* let tc_ioannidis = ioannidis_tc r k in
           let fml_tc_ioannidis = self#visit_exp subst tc_ioannidis tuple in *)
        let term, fml = (tc_square, fml_tc_square) in
        Msg.debug (fun m -> m "TC term: %a" E.(pp_exp (arity term)) term);
        fml

      method build_Transpose (_ : stack) _ r' tuple =
        r' @@ Tuple.transpose tuple

      method build_True (_ : stack) = true_
      method build_U (_ : stack) (a : ltl) (b : ltl) : ltl = until a b
      method build_Union (_ : stack) _ _ e1 e2 x = e1 x +|| lazy (e2 x)
      method build_Univ (_ : stack) __tuple = true_
      method build_Mul (_ : stack) t1 t2 = mul t1 t2

      method build_Lshift (_ : stack) t1 t2 =
        match env#shl_list with
        | [] -> assert false
        | triples -> create_shift_formula t1 t2 triples

      method build_Zershift (_ : stack) t1 t2 =
        match env#shr_list with
        | [] -> assert false
        | triples -> create_shift_formula t1 t2 triples

      method build_Sershift (_ : stack) t1 t2 =
        match env#sha_list with
        | [] -> assert false
        | triples -> create_shift_formula t1 t2 triples

      (* FROM Alloy util/integer:
         *
         * Performs the division with "round to zero" semantics, except the following 3 cases
         * 1) if a is 0, then it returns 0
         * 2) else if b is 0, then it returns 1 if a is negative and -1 if a is positive
         * 3) else if a is the smallest negative integer, and b is -1, then it returns a
      *)
      method build_Div (_ : stack) a b =
        let zero = num env#bitwidth 0 in
        let one = num env#bitwidth 1 in
        let minus_one = num env#bitwidth ~-1 in
        let min_int = num env#bitwidth Int.(~-2 ** (env#bitwidth - 1)) in
        let eq_ = comp eq in
        ifthenelse_arith (eq_ a zero) zero
        @@ ifthenelse_arith
             (and_ (eq_ b zero) @@ lazy (comp lt a zero))
             minus_one
        @@ ifthenelse_arith (and_ (eq_ b zero) @@ lazy (comp gt a zero)) one
        @@ ifthenelse_arith (and_ (eq_ a min_int) @@ lazy (eq_ b minus_one)) a
        @@ div a b

      (* like build_Div *)
      method build_Rem (_ : stack) a b =
        let zero = num env#bitwidth 0 in
        let minus_one = num env#bitwidth ~-1 in
        let min_int = num env#bitwidth Int.(~-2 ** (env#bitwidth - 1)) in
        let eq_ = comp eq in
        ifthenelse_arith (eq_ a zero) zero
        @@ ifthenelse_arith (eq_ b zero) a
        @@ ifthenelse_arith
             (and_ (eq_ a min_int) @@ lazy (eq_ b minus_one))
             zero
        @@ rem a b

      (* due to the presence of a bound variable, the recursion over the body  done by the visitor is "wrong"*)
      method! visit_Sum subst _visitors_c0 _visitors_c1 =
        let _visitors_r0 = self#visit_'exp subst _visitors_c0 in
        (* min_int as a dummy default value to ignore later on *)
        let _visitors_r1 = num env#bitwidth 0 in
        self#build_Sum subst _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

      (* [| sum x : r | ie |]_sigma =
         Sum_{t in must(r)} [|ie|]_sigma[x |-> t]
         + Sum_{t in may(r)} ([|r|]_sigma(t) => [|ie|]_sigma[x |-> t] else 0)

         (due to the presence of a bound variable, the recursion over the body  done by the visitor is "wrong" => last argument ignored)
      *)
      method build_Sum (subst : stack) (r : E.exp) ie _ _ =
        let { must; may; _ } = env#must_may_sup subst r in
        let ie' t = self#visit_iexp (t :: subst) ie in
        let must_part = summation ~bw:env#bitwidth ~on:must ie' in
        let may_part =
          summation ~bw:env#bitwidth ~on:may (fun t ->
              ifthenelse_arith (self#visit_exp subst r t) (ie' t)
                (num env#bitwidth 0))
        in
        plus must_part may_part

      (* [|#e|]_sigma = size(must(e)) + Sum_{t in may(e)} ([|e|]_sigma(t) => 1 else 0) *)
      method build_Card subst r r' =
        let { must; may; _ } = env#must_may_sup subst r in
        let must_card = num env#bitwidth @@ TS.size must in
        let may_part =
          summation ~bw:env#bitwidth ~on:may (fun t ->
              ifthenelse_arith (r' t) (num env#bitwidth 1) (num env#bitwidth 0))
        in
        plus must_card may_part

      (* [| Int[ie] |]_s(t) = [|ie|]_s = to_int(t) if t in Int
         [| Int[ie] |]_s(t) = false otherwise *)
      method build_Big_int (_ : stack) (_a : E.iexp) (ie' : term) t : ltl =
        match env#int_of_tuple t with
        | None -> false_
        | Some n -> comp eq ie' (num env#bitwidth n)

      (* [|int[e]|] = SUM_{t \in up(Int)}([|e|](t) => extract_int(t) else 0)  *)
      method build_Small_int _e e' =
        summation ~bw:env#bitwidth ~on:env#int_set @@ fun int_tuple ->
        ifthenelse_arith (e' int_tuple)
          (num env#bitwidth @@ Option.get_exn_or __LOC__
         @@ env#int_of_tuple int_tuple)
          (num env#bitwidth 0)

      (* FIXME *)
      method build_Var (subst : stack) idx _ tuple =
        match List.get_at_idx idx subst with
        | None ->
            Fmtc.kstr failwith "%s.build_Var: variable %d not found in %a"
              __MODULE__ idx pp_subst subst
        | Some value -> if Tuple.equal value tuple then true_ else false_

      method build_X (_ : stack) (a : ltl) : ltl = next a
      method build_oexp (_ : stack) __e e' __ar tuple = e' tuple
    end

  (* class *)

  let formula_as_comment fml =
    let str = Fmt.to_to_string (Elo.pp_fml 0) fml in
    "-- " ^ String.replace ~which:`All ~sub:"\n" ~by:"\n-- " str

  (* Converts an Ast formula to an LTL formula, gathering at the same time the
     rigid and flexible variables having appeared during the walk. *)
  let convert elo elo_fml =
    let comment = formula_as_comment elo_fml in
    (* Msg.debug (fun m ->
        m
          "----------------------------------------------------------------------\n\
           %s"
          comment); *)
    (* let before_conversion = Mtime_clock.now () in *)
    (* Msg.info (fun m -> m "DOMAIN@\n%a@." Domain.pp elo.Elo.domain); *)
    let env = new environment elo in
    let ltl_fml = (new converter env)#visit_fml [] elo_fml in
    (* let conversion_time = Mtime.span before_conversion @@ Mtime_clock.now () in *)
    (* Msg.debug (fun m ->
        m "Conversion done in %a@." Mtime.Span.pp conversion_time); *)
    (comment, ltl_fml)
end
