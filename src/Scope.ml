
type t =
  | Exact of Bound.t              
  | Inexact of Bound.t * Bound.t    

let exact bound = Exact bound

let inexact inf sup =
  assert Bound.(arity inf = arity sup
                || Bound.is_empty inf);
  Inexact (inf, sup)

let arity = function
  | Exact b
  | Inexact (_, b) -> Bound.arity b

let pp out = function
  | Exact bound -> Bound.pp out bound
  | Inexact (inf, sup) ->
      Fmtc.(pair ~sep:sp (box2 Bound.pp) (box2 Bound.pp)) out (inf, sup)
        
module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
