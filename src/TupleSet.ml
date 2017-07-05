[@@@landmark "auto"]
open Containers
    
module TS = Tuple.Set

type t = TS.t

let pp out b =
  Fmtc.pf out "@[<hov 2>{";
  TS.pp (* ~start:"" ~stop:"" *) ~sep:" " Tuple.pp out b;
  Fmtc.pf out "}@]"

module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 

let to_list = TS.elements
                
let to_seq = TS.to_seq

let of_seq = TS.of_seq
               
(* let empty = TS.empty *)
let empty () = TS.create 29

let of_tuples tuples =
  match tuples with
    | [] -> empty ()
    | t :: ts ->
        let ar = Tuple.arity t in
        assert (List.for_all (fun t2 -> Tuple.arity t2 = ar) ts);
        TS.of_list tuples 

let is_empty b =
  to_list b = []


(* let inferred_arity b = *)
(*   if is_empty b then 0 *)
(*   else Tuple.arity @@ TS.choose b *)

let inferred_arity b =
  if is_empty b then 0
  else Tuple.arity @@ Sequence.head_exn @@ TS.to_seq b

let tuples t = t

let inter b1 b2 =
  TS.inter b1 b2

let size bnd =
  TS.cardinal bnd

let subset b1 b2 =
  TS.subset b1 b2

let equal b1 b2 =
  TS.equal b1 b2

(* let compare b1 b2 = *)
(*   TS.compare b1 b2 *)
    
let compare b1 b2 =
  if equal b1 b2 then 0
  else if subset b1 b2 then -1
  else 1
    
let product b1 b2 =
  if is_empty b1 then
    b2
  else if is_empty b2 then
    b1
  else
    Sequence.product (TS.to_seq @@ tuples b1) (TS.to_seq @@ tuples b2)
    |> Sequence.map Fun.(uncurry Tuple.(@@@))
    |> TS.of_seq

let union b1 b2 =
  TS.union b1 b2 

let diff b1 b2 = 
  CCCache.(with_cache (lru ~eq:equal 256) TS.diff b1) b2

(* let transpose b = *)
(*   let ar = inferred_arity b in *)
(*   assert (ar = 2 || ar = 0); *)
(*   TS.map Tuple.transpose b *)

let map f ts =
  TS.to_seq ts
  |> Sequence.map f
  |> TS.of_seq


(* let filter = TS.filter *)

let filter test ts =
  TS.fold (fun acc tuple ->
        if test tuple then (TS.insert acc tuple; acc)
        else acc) (empty ()) ts


let transpose b =
  let ar = inferred_arity b in
  assert (ar = 2 || ar = 0);
  map Tuple.transpose b

(* r ++ s (so we need the first column of s) *)
let override r s =
  let in_r_but_not_in_s1 =
    filter
      (fun tr ->
         not @@ TS.exists
                  (fun ts1 -> Tuple.(ith 0 tr = ith 0 ts1)) s) r
  in
  TS.union s in_r_but_not_in_s1

(* [s <: r] *)
let lproj s r =
  filter (fun tr -> TS.mem s Tuple.([ith 0 tr] |> of_list1)) r

let rproj r s = lproj s @@ transpose r

let diagonal b =
  map Tuple.(fun e -> e @@@ e) b

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


(* computes the transitive closure of tue tuple set b using iterative squares *)        
let transitive_closure_is b =
  assert (inferred_arity b = 2 || inferred_arity b = 0);
  let old = ref b in
  let cur = ref (union b (join b b)) in
  while not @@ TS.equal !old !cur do
    old := !cur;
    cur := union !cur (join !cur !cur);
    (* Msg.debug (fun m -> *)
    (*     m "current 2 =  %a " pp !cur); *)
    (* Msg.debug (fun m -> *)
    (*     m "old 2 =  %a " pp !old); *)
    (* Msg.debug (fun m -> m "egalité? %b " (TS.equal !old !cur)) *)
  done;
  !cur

let transitive_closure b =
  Msg.debug (fun m -> m "arité: %d" (inferred_arity b));
  assert (inferred_arity b = 2 || inferred_arity b = 0);
  let old = ref b in
  let cur = ref (union b (join b b)) in
  let b_to_the_k = ref (join b b) in
  while not @@ TS.equal !old !cur do
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

(* let mem_aux (t, bnd) = *)
(*   TS.mem t bnd *)

(* let mem t bnd = *)
(*   CCCache.(with_cache *)
(*              (lru ~eq:(Pair.equal Tuple.equal equal) *)
(*                 ~hash:(Hash.pair Tuple.hash hash) 597) *)
(*              mem_aux) (t, bnd) *)

let mem t bnd =
  TS.mem bnd t

  
