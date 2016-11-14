open Containers
    
module TS = Tuple.Set

type t = {
  tuples : TS.t;
  arity : int
}

let empty_arity = 0

let empty = {
  tuples = TS.empty;
  arity = empty_arity
}

let of_tuples tuples = match tuples with
  | [] -> empty
  | t :: ts ->
      let ar = Tuple.arity t in
      assert (List.for_all (fun t2 -> Tuple.arity t2 = ar) ts);
      {
        arity = ar;
        tuples = TS.of_list tuples 
      }

let arity { arity; _ } = arity

let tuples { tuples; _ } = tuples

let is_empty b =
  arity b = empty_arity

let inter b1 b2 =
  assert (b1.arity = b2.arity);
  {
    tuples = TS.inter b1.tuples b2.tuples;
    arity = b1.arity
  }

let size bnd =
  TS.cardinal bnd.tuples

let subset b1 b2 =
  assert (b1.arity = b2.arity || b1.arity = empty_arity);
  TS.subset b1.tuples b2.tuples

let equal b1 b2 =
  assert (b1.arity = b2.arity || b1.arity = empty_arity);
  TS.equal b1.tuples b2.tuples

let compare b1 b2 =
  TS.compare b1.tuples b2.tuples
    
let product b1 b2 =
  if is_empty b1 then
    b2
  else if is_empty b2 then
    b1
  else
    {
      arity = arity b1 + arity b2;
      tuples = Sequence.product (TS.to_seq @@ tuples b1) (TS.to_seq @@ tuples b2)
               |> Sequence.map Tuple.append
               |> TS.of_seq
    }




  let union b1 b2 =
    assert (b1.arity = b2.arity);
    { b1 with tuples = TS.union b1.tuples b2.tuples }
 

let mem t bnd = TS.mem t bnd.tuples

let pp out { tuples; _ } =
  TS.print ~start:"{" ~stop:"}" ~sep:" " Tuple.pp out tuples
  
module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
