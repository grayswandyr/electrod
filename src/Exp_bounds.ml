(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers

module G = Elo
module TS = Tuple_set

type bounds = {
  must : TS.t;
  sup : TS.t;
  may : TS.t;
}

let pp_subst out subst =
  Fmtc.(brackets @@ list @@ parens @@ pair int Tuple.pp) out
    (List.mapi (fun i tuple -> (i, tuple)) subst)


(* says whether all tuples in a list are different from one another *)
let rec alldiff (tuples : Tuple.t list) : bool = match tuples with
  | [] -> true
  | hd::tl when List.mem ~eq:Tuple.equal hd tl -> false
  | _::tl -> alldiff tl 

(* [sets] is a list of tuple sets, but a tuple set is expected to be given as a list of tuples. Returns a list of tuple lists: each of these represents a single tuple in the cartesian product, but not concatenated into a single long tuple yet (because these tuples will have to be composed with others before creating a really-long tuple) 

   E.g.: suppose `set = [(1, 2); (3, 4)]`. 
   Then we obtain, for `n = 3`:

   [
   [(3, 4); (3, 4); (3, 4)]; 
   [(3, 4); (3, 4); (1, 2)]; 
   [(3, 4); (1, 2); (3, 4)];
   [(3, 4); (1, 2); (1, 2)]; 
   [(1, 2); (3, 4); (3, 4)]; 
   [(1, 2); (3, 4); (1, 2)];
   [(1, 2); (1, 2); (3, 4)]; 
   [(1, 2); (1, 2); (1, 2)]
   ] 
*)

let nproduct 
      (n : int) (disj : bool) (set : Tuple.t list) : Tuple.t list list = 
  List.repeat n [set]
  (* returns (a list of) lists of tuples: *)
  |> List.cartesian_product  
  (* possibly remove such lists that contain several instance of the same tuple *)
  |> if disj then List.filter alldiff else Fun.id 

(* In all the following functions, [subst] is a *stack* ot tuples, meaning 
   tuples (corresponding to DB indices) appear reversed wrt their order of binding. Then index 0 refers to the last (nearest) binding. *)
let make_bounds_exp =
  let return_bounds (exp, subst) must sup =
    (if not (TS.subset must sup) then
       Msg.err (fun m ->
             m
               "%s.bounds@ %a@ with@\nsubst= %a@\n@\nmust(%a)=@ \
                %a@\nsup(%a)=@ %a@."
               __FILE__
               (G.pp_exp @@ List.length subst) exp
               pp_subst subst
               (G.pp_exp @@ List.length subst) exp
               TS.pp must
               (G.pp_exp @@ List.length subst) exp
               TS.pp sup)
    );
    { must; sup; may = TS.diff sup must }    
  in 
  let eq x1 x2 = 
    CCEqual.(pair 
               physical
               (list Tuple.equal)) x1 x2
  in
  let hash x = 
    Hash.(pair 
            (fun (G.Exp { hkey; _ }) -> hkey)
            (list Tuple.hash) ) x
  in 
  (* let cb ~in_cache (e, subst) __value =
     Fmtc.(pr "@[<h>Cache search for %a: %B@]@\n" 
     (pair ~sep:sp (brackets @@ list ~sep:comma Tuple.pp) 
     (G.pp_exp (List.length subst))) 
     (subst, e)
     in_cache)
     in *)
  let cache = CCCache.unbounded ~eq ~hash 1793 in
  fun domain ->
    CCCache.with_cache_rec cache 
      begin 
        fun fbounds_exp 
          (G.Exp { node = { prim_exp = pe; _ }; _ }, subst as args) ->
          let open G in 
          match pe with         
            | Var v -> 
                begin match List.get_at_idx v subst with
                  | None ->
                      Fmt.kstrf failwith 
                        "%s.bounds_prim_exp: no variable %d in substitution %a" __FILE__ 
                        v
                        pp_subst subst
                  | Some tuple ->
                      let singleton =
                        TS.singleton tuple
                      in
                      return_bounds args singleton singleton
                end
            | Name n -> 
                let rel = Domain.get_exn n domain in
                return_bounds args (Relation.must rel) (Relation.sup rel)
            | None_ ->
                return_bounds args TS.empty TS.empty
            | Univ ->
                let univ = Domain.univ_atoms domain in
                return_bounds args univ univ
            | Iden ->
                let iden = Domain.get_exn Name.iden domain in
                return_bounds args (Relation.must iden) (Relation.sup iden)
            | RUn (Transpose, e) ->
                let b = fbounds_exp (e, subst) in
                return_bounds args (TS.transpose b.must) (TS.transpose b.sup)
            | RUn (TClos, e) -> 
                let b = fbounds_exp (e, subst) in
                return_bounds args (TS.transitive_closure b.must) (TS.transitive_closure b.sup)
                |> Fun.tap
                     (fun res ->
                        Msg.debug (fun m ->
                              m
                                "bounds_prim_exp(TClos):@\n\
                                 must(%a) = %a@\nmay(%a) = %a"
                                (G.pp_prim_exp @@ List.length subst) pe
                                TS.pp res.must
                                (G.pp_prim_exp @@ List.length subst) pe
                                TS.pp res.may))
            | RUn (RTClos, e) -> 
                let iden = Domain.get_exn Name.iden domain in
                let b = fbounds_exp (e, subst) in
                return_bounds args (TS.union (TS.transitive_closure b.must) @@ Relation.must iden)
                  (TS.union (TS.transitive_closure b.sup) @@ Relation.sup iden)
                |> Fun.tap
                     (fun res ->
                        Msg.debug (fun m ->
                              m
                                "bounds_prim_exp(TClos):@\n\
                                 must(%a) = %a@\nmay(%a) = %a"
                                (G.pp_prim_exp @@ List.length subst) pe
                                TS.pp res.must
                                (G.pp_prim_exp @@ List.length subst) pe
                                TS.pp res.may))

            | RBin (e1, Union ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args (TS.union b1.must b2.must) (TS.union b1.sup b2.sup)
            | RBin (e1, Inter ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args (TS.inter b1.must b2.must) (TS.inter b1.sup b2.sup)
            | RBin (e1, Over ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args (TS.override b1.must b2.must) (TS.override b1.sup b2.sup)
            | RBin (e1, LProj ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args (TS.lproj b1.must b2.must) (TS.lproj b1.sup b2.sup)
            | RBin (e1, RProj ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args (TS.rproj b1.must b2.must) (TS.rproj b1.sup b2.sup)
            | RBin (e1, Prod ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args (TS.product b1.must b2.must) (TS.product b1.sup b2.sup)
            | RBin (e1, Diff ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                (* compute the tuples that are necessarily in e1 and necessarily not in e2 *)
                let must_diff = TS.filter (fun tup -> not (TS.mem tup b2.sup)) b1.must in
                return_bounds args must_diff (TS.diff b1.sup b2.must) (* b2.MUST! *)
            | RBin (e1, Join ,e2) -> 
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args (TS.join b1.must b2.must) (TS.join b1.sup b2.sup)
            | RIte (_, e1, e2) ->
                let b1 = fbounds_exp (e1, subst) in
                let b2 = fbounds_exp (e2, subst) in
                return_bounds args 
                  (TS.inter b1.must b2.must) (TS.union b1.sup b2.sup) 
            | Prime e ->
                fbounds_exp (e, subst)
            | Compr (sim_bindings, _) ->
                (* The must of a compr set is empty. Indeed, it would be wrong
                   to compute the must from the sim_bindings only, because the
                   formula could be in contradiction with this must. Taking the
                   formula into account is not possible, so we consider an empty
                   must. *)
                let sup_sim_binding
                      (subst : Tuple.t list) (disj, nbvars, range)
                  : Tuple.t list list = 
                  let { sup; _ } = fbounds_exp (range, subst) in
                  let sup_as_list = TS.to_list sup in 
                  (* compute the exponent for this bindings *)
                  nproduct nbvars disj sup_as_list  
                in 
                let rec sup_sim_bindings 
                          (subst : Tuple.t list) sim_bindings 
                  : Tuple.t list = 
                  match sim_bindings with 
                    | [] -> assert false
                    | [sim_binding] -> 
                        (* compute the product of bounds for the head sim_binding *)
                        sup_sim_binding subst sim_binding
                        (* base case => convert back into a list of tuples *)
                        |> List.map Tuple.concat 
                    | hd::tl -> 
                        let hd_sup = sup_sim_binding subst hd in 
                        (* as tail bindings may depend on the head, update the substitution *)   

                        List.fold_left 
                          (fun acc line -> 
                             acc @ sup_sim_bindings (line @ subst) tl)
                          []
                          hd_sup
                in

                let sup_list = sup_sim_bindings subst sim_bindings in 
                return_bounds args TS.empty (TS.of_tuples sup_list)

                |> Fun.tap (fun { sup; _} -> 
                      Msg.info (fun m -> 
                            m "bounds Compr %a %a = %a@."
                              (Elo.pp_prim_exp 0) pe 
                              pp_subst subst 
                              TS.pp sup
                          ))

      end

(* Computes the bounds for a sim_binding, in the case of a set defined by
   comprehension.

   The whole idea of this function (and the following auxiliary ones) is to
   apply the following scheme.  Say we want to compute: 

   must({x : s, y : x.r | ...})

   The result must be:

   must(s) -> { UNION_(tuple in must(s)) must( x.r [tuple/x] ) }

   as every pair in the comprehension is s.t. the range 
   for y depends on the value for x.

   E.g.: Taking must(s) = { a b }, must(r) = { (a b) (b c) (a c) (a d) }:

   x : s, y : x.r

   should yield:

   (a x a.r) union (b x b.r) = {(a b) (a c) (a d) (b c)}

   So what we do is the following: Walk left-to-right along some sim_bindings
   and compute the corresponding bound, while accumulating already computed
   bounds. Finally everything is composed.  

   [subst] is a substitution from already-visited sim_bindings. [fbound] is the
   function returning the bound (say the must or the sup).  *)

(* and bounds_sim_bindings
   fbound
   domain
   (subst : Tuple.t list)
   (sbs : (G.var, G.ident) G.sim_binding list)
   : Tuple.t list =  
   let open! List in
   match sbs with
   | [] -> assert false      (* nonempty list *)
   | [(disj, vs, r)] -> 
   (* compute the product of bounds for the head sim_binding, it yields a
   list of combinations (= lists) of tuples *)
   bounds_sim_binding fbound domain subst disj vs r
   (* as we're done, we concat tuples in every combination *)
   >|= Tuple.concat
   | (disj, vs, r)::tl ->
   (* compute the product of bounds for the head sim_binding, it yields a
   list of combinations (= lists) of tuples *)
   let hd_bnd =
   bounds_sim_binding fbound domain subst disj vs r
   in
   (* cast to create a substitution below *)
   let vars = map (fun (G.BVar v) -> v) vs in
   (* as explained above in the example: for every possible combination
   of tuples in [hd_bnd], we have to substitute *this* combination in
   the tail, then compute the resulting product and add it to the
   whole possibilities *)
   fold_left
   (fun acc combination ->
   acc
   @ (* we use @ instead of union because it should be faster and
   the end result will be converted back into a set anyway *)
   product_for_one_hd_combination
   fbound domain subst vars tl combination)
   []                  (* empty accumulator *)
   hd_bnd

   (* given the list of tuples corresponding to the bound for the head sim
   binding (so it's a list of tuples, as there are multiple variables bound
   to a given range), compute all the products with all the results obtained
   by substituting in the tail according to the substitutions induced by the
   said head tuples *)
   and product_for_one_hd_combination fbound domain subst vars
   (tail : (G.var, G.ident) G.sim_binding list)
   (hd_combination : Tuple.t list)
   : Tuple.t list =
   let open! List in
   (* compute the corresponding substitution for this instance of the head bound *)
   let subst2 = combine vars hd_combination @ subst in
   (* compute all the tuples for the tail with *this* subtitution *)
   let tl_bnd =
   bounds_sim_bindings fbound domain subst2 tail
   in
   (* create all combinations with this instance of the head bound and the
   tuples computed for the tail *)
   product Tuple.(@@@) [Tuple.concat hd_combination] tl_bnd *)

(* for one sim_binding *)
(* and __bounds_sim_binding 
   fbound
   domain
   (subst : Tuple.t list) 
   disj
   vars
   (dom : G.exp)
   : Tuple.t list list
   =
   let open List in
   (* compute the bound for the range of *this* sim binding. NOTE: [range_bnd]
   is given as a [Tuple.t list] instead of a [TS.t] for technical purposes
   only. As a consequence, the returned value for the whole function is a
   set of sets of tuples represented by a list of lists... *)
   let range_bnd =
   TS.to_list @@ fbound @@ bounds_exp subst domain dom
   in
   (* compute the bound for the whole head sim binding  *)
   let lg = length vars in
   let prod =
   (* create as many copies as necessary (= nb of variables) of the domain *)
   init lg (fun _ -> range_bnd)
   (* & compute product (NOTE: tuples are not concatenated, just put in the
   same list representing a combination of tuples).  

   E.g.: suppose `range_bnd = [(1, 2); (3, 4)]`. 
   Then we obtain, for `lg = 3`:

   [
   [(3, 4); (3, 4); (3, 4)]; 
   [(3, 4); (3, 4); (1, 2)]; 
   [(3, 4); (1, 2); (3, 4)];
   [(3, 4); (1, 2); (1, 2)]; 
   [(1, 2); (3, 4); (3, 4)]; 
   [(1, 2); (3, 4); (1, 2)];
   [(1, 2); (1, 2); (3, 4)]; 
   [(1, 2); (1, 2); (1, 2)]
   ] *)
   |> cartesian_product 
   in
   (* Remove lines where there are tuples in common if [disj = true].
   [all_different] is defined below. *)
   (if disj then filter __all_different prod else prod)
   (* |> Fun.tap *)
   (* @@ fun res -> *)
   (* Msg.debug *)
   (*   (fun m -> *)
   (*      m "bounds_sim_binding %B %a %a --> @[<hov>%a@]" *)
   (*        disj *)
   (*        Fmtc.(list ~sep:(const string ", ")@@ G.pp_var) vars *)
   (*        Fmtc.(list ~sep:sp @@ Tuple.pp) range_bnd *)
   (*        Fmtc.(brackets @@ list ~sep:sp @@ brackets @@ list ~sep:sp @@ Tuple.pp) res) *)
*)
