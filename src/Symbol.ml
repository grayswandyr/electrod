open Containers

module H = Hashcons

type t = string H.hash_consed

module S = H.Make(struct
    type t = string
    let hash = String.hash
		let equal = String.equal 
  end)
  
(* ********************* *)
(* table for hashconsing *)
(* ********************* *)
let table = S.create 271
(* ********************* *)
           
let make s =
  S.hashcons table s

let hash sym =
  sym.H.hkey

let compare s1 s2 =
  s1.H.tag - s2.H.tag

let equal x1 x2 =
  x1 == x2

let pp out at =
  (* Format.fprintf out "%s/%d" at.H.node at.H.tag *)
  Format.fprintf out "%s" at.H.node 

include Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
