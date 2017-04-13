open Containers
    
module TS = Tuple.Set

type t = TS.t


let empty = TS.empty

let of_tuples tuples = match tuples with
  | [] -> empty
  | t :: ts ->
      let ar = Tuple.arity t in
      assert (List.for_all (fun t2 -> Tuple.arity t2 = ar) ts);
      TS.of_list tuples 

let inferred_arity b =
  if TS.is_empty b then 0
  else Tuple.arity @@ TS.choose b

let tuples t = t

let is_empty b =
  TS.is_empty b

let inter b1 b2 =
  TS.inter b1 b2

let size bnd =
  TS.cardinal bnd

let subset b1 b2 =
  TS.subset b1 b2

let equal b1 b2 =
  TS.equal b1 b2

let compare b1 b2 =
  TS.compare b1 b2
    
let product b1 b2 =
  if is_empty b1 then
    b2
  else if is_empty b2 then
    b1
  else
    Sequence.product (TS.to_seq @@ tuples b1) (TS.to_seq @@ tuples b2)
    |> Sequence.map Tuple.append
    |> TS.of_seq


let union b1 b2 =
  assert (inferred_arity b1 = inferred_arity b2);
  TS.union b1 b2 
 

let mem t bnd = TS.mem t bnd

let pp out b =
  Fmtc.pf out "@[<2>{";
  TS.pp ~start:"" ~stop:"" ~sep:" " Tuple.pp out b;
  Fmtc.pf out "}@]"
  
module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
