open Containers
    
module TS = Tuple.Set

type t = TS.t

let pp out b =
  Fmtc.pf out "@[<hov 2>{";
  TS.pp ~start:"" ~stop:"" ~sep:" " Tuple.pp out b;
  Fmtc.pf out "}@]"


let to_seq = TS.to_seq

let of_seq = TS.of_seq
               
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
    |> Sequence.map Tuple.(@@@)
    |> TS.of_seq


let union b1 b2 =
  TS.union b1 b2 

let diff b1 b2 = 
  TS.diff b1 b2

let transpose b =
  let ar = inferred_arity b in
  assert (ar = 2 || ar = 0);
  TS.map Tuple.transpose b


let diagonal b =
  TS.map (fun e -> Tuple.(@@@) (e, e)) b

let join b1 b2 =
  let open Sequence in
  let lg1 = inferred_arity b1 in
  let s1 = to_seq b1 in
  let s2 = to_seq b2 in
  product s1 s2
  |> filter_map
       (fun (t1, t2) ->
          if Atom.equal (Tuple.ith (lg1 - 1) t1) (Tuple.ith 0 t2)
          then Some (Tuple.join t1 t2)
          else None)
  |> of_seq

let mem t bnd = TS.mem t bnd

let filter = TS.filter

  
module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 

module Infix = struct
  let ( $: ) = mem
end