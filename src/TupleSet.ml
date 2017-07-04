[@@@landmark "auto"]
open Containers
    
module TS = Tuple.Set

type t = {
  contents : TS.t;
  hash : int
}

let pp out b =
  Fmtc.pf out "@[<hov 2>{";
  TS.pp (* ~start:"" ~stop:"" *) ~sep:" " Tuple.pp out b.contents;
  Fmtc.pf out "}@]"

module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 

let to_list ts = TS.elements ts.contents
                
let to_seq ts = TS.to_seq ts.contents

let hash_tuples_set set =
  Hash.seq Tuple.hash @@ TS.to_seq set

let of_set contents = {
  contents;
  hash = hash_tuples_set contents
}
let of_seq seq =
  of_set @@ TS.of_seq seq

(* let empty = TS.empty *)
let empty =
  of_set TS.empty
    

let of_tuples tuples =
  match tuples with
    | [] -> empty 
    | t :: ts ->
        let ar = Tuple.arity t in
        assert (List.for_all (fun t2 -> Tuple.arity t2 = ar) ts);
        of_set @@ TS.of_list tuples 

let is_empty b =
  TS.is_empty b.contents


(* let inferred_arity b = *)
(*   if is_empty b then 0 *)
(*   else Tuple.arity @@ TS.choose b *)

let inferred_arity b =
  if is_empty b then 0
  else Tuple.arity @@ Sequence.head_exn @@ TS.to_seq b.contents

let tuples t = t.contents

let hash t = t.hash

let inter b1 b2 =
  of_set @@ TS.inter b1.contents b2.contents

let size bnd =
  TS.cardinal bnd.contents

let subset b1 b2 =
  TS.subset b1.contents b2.contents

let equal b1 b2 =
  TS.equal b1.contents b2.contents

(* let compare b1 b2 = *)
(*   TS.compare b1 b2 *)
    
let compare b1 b2 =
  TS.compare b1.contents b2.contents
    
let product b1 b2 =
  if is_empty b1 then
    b2
  else if is_empty b2 then
    b1
  else
    Sequence.product (TS.to_seq b1.contents) (TS.to_seq b2.contents)
    |> Sequence.map Fun.(uncurry Tuple.(@@@))
    |> of_seq

let union_aux (b1, b2) =
  of_set @@ TS.union b1.contents b2.contents

let union =
  Fun.curry
  @@ CCCache.(with_cache (lru ~eq:(Pair.equal equal equal)
                            ~hash:(Hash.pair hash hash) 253) @@ union_aux)


let diff_aux (b1, b2) =
  of_set @@ TS.diff b1.contents b2.contents

let diff =
  Fun.curry
  @@ CCCache.(with_cache (lru ~eq:(Pair.equal equal equal)
                            ~hash:(Hash.pair hash hash) 253) @@ diff_aux)





let map f ts =
  of_set @@ TS.map f ts.contents 
  
  
let transpose b =
  let ar = inferred_arity b in
  assert (ar = 2 || ar = 0);
  map Tuple.transpose b

let filter f ts =
  of_set @@ TS.filter f ts.contents


(* let mem_aux (t, bnd) = *)
(*   TS.mem t bnd *)

(* let mem t bnd = *)
(*   CCCache.(with_cache *)
(*              (lru ~eq:(Pair.equal Tuple.equal equal) *)
(*                 ~hash:(Hash.pair Tuple.hash hash) 597) *)
(*              mem_aux) (t, bnd) *)

let mem t bnd =
  TS.mem t bnd.contents

(* r ++ s (so we need the first column of s) *)
let override r s =
  let in_r_but_not_in_s1 =
    TS.filter
      (fun tr ->
         not
         @@ TS.exists
              (fun ts1 -> Tuple.(ith 0 tr = ith 0 ts1)) s.contents) r.contents
  in
  of_set @@ TS.union s.contents in_r_but_not_in_s1

(* [s <: r] *)
let lproj s r =
  of_set
  @@ TS.filter (fun tr ->
        TS.mem Tuple.([ith 0 tr] |> of_list1) s.contents) r.contents

let rproj r s = lproj s @@ transpose r

let diagonal b =
  map Tuple.(fun e -> e @@@ e) b

let join_aux (b1, b2) =
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

let join =
  Fun.curry
  @@ CCCache.(with_cache (lru ~eq:(Pair.equal equal equal)
                            ~hash:(Hash.pair hash hash) 253) @@ join_aux)


(* computes the transitive closure of tue tuple set b using iterative squares *)        
let transitive_closure_is b =
  assert (inferred_arity b = 2);
  let old = ref b in
  let cur = ref (union b (join b b)) in
  while not @@ equal !old !cur do
    old := !cur;
    cur := union !cur (join !cur !cur);
    (* Msg.debug (fun m -> *)
    (*     m "current 2 =  %a " pp !cur); *)
    (* Msg.debug (fun m -> *)
    (*     m "old 2 =  %a " pp !old); *)
    (* Msg.debug (fun m -> m "egalité? %b " (TS.equal !old !cur)) *)
  done;
  !cur

let transitive_closure_aux b =
  assert (inferred_arity b = 2);
  let old = ref b in
  let cur = ref (union b (join b b)) in
  let b_to_the_k = ref (join b b) in
  while not @@ equal !old !cur do
    old := !cur;
    b_to_the_k := join b !b_to_the_k;
    cur := union !cur !b_to_the_k;
    (* Msg.debug (fun m -> *)
    (*     m "current 2 =  %a " pp !cur); *)
    (* Msg.debug (fun m -> *)
    (*     m "old 2 =  %a " pp !old); *)
    (* Msg.debug (fun m -> m "egalité? %b " (TS.equal !old !cur)) *)
  done;
  !cur

let transitive_closure =
  CCCache.(with_cache (lru ~eq:(equal)
                         ~hash:(hash) 253) @@ transitive_closure_aux)


  
