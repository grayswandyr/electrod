open Containers
    
type t =
  | Exact of TupleSet.t              
  | Inexact of TupleSet.t * TupleSet.t    

let exact bound = Exact bound

let inexact inf sup =
  assert TupleSet.(inferred_arity inf = inferred_arity sup
                || TupleSet.is_empty inf);
  Inexact (inf, sup)

let inferred_arity = function
  | Exact b
  | Inexact (_, b) -> TupleSet.inferred_arity b


let included_in tupleset = function
  | Exact exact ->
      TupleSet.subset tupleset exact
  | Inexact (inf, sup) -> 
      TupleSet.subset inf tupleset
      && TupleSet.subset tupleset sup

let inf = function
  | Exact ts
  | Inexact (ts, _) -> ts
    
let sup = function
  | Exact ts
  | Inexact (_, ts) -> ts

let must = inf

let may_aux sc =
  assert (TupleSet.subset (inf sc) (sup sc));
  match sc with
    | Exact _ ->
        TupleSet.empty 
    | Inexact (inf, sup) ->
        TupleSet.diff sup inf

let equal sc1 sc2 = match sc1, sc2 with
  | Exact ts1, Exact ts2 -> TupleSet.equal ts1 ts2
  | Inexact (inf1, sup1), Inexact (inf2, sup2) ->
      TupleSet.(equal inf1 inf2 && equal sup1 sup2)
  | Exact _, Inexact _
  | Inexact _, Exact _ -> false

let may =
  CCCache.(with_cache (lru ~eq:equal 253) may_aux) 


let pp out = function
  | Exact bound -> TupleSet.pp out bound
  | Inexact (inf, sup) ->
      Fmtc.(box @@ pair ~sep:sp (box2 TupleSet.pp) (box2 TupleSet.pp)) out (inf, sup)
        

 

module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
 

