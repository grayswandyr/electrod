
type t =
  | Exact of TupleSet.t              
  | Inexact of TupleSet.t * TupleSet.t    

let exact bound = Exact bound

let inexact inf sup =
  assert TupleSet.(arity inf = arity sup
                || TupleSet.is_empty inf);
  Inexact (inf, sup)

let arity = function
  | Exact b
  | Inexact (_, b) -> TupleSet.arity b

let pp out = function
  | Exact bound -> TupleSet.pp out bound
  | Inexact (inf, sup) ->
      Fmtc.(pair ~sep:sp (box2 TupleSet.pp) (box2 TupleSet.pp)) out (inf, sup)
        
module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
