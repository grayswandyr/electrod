open Containers

(* Hashcons_util stands for Hashcons, see Hashcons_util.ml *)
type t = string Hashcons.hash_consed

module H = Hashcons.Make(struct
    type t = string
    let hash = String.hash
		let equal = String.equal 
  end)
  
(* ********************* *)
(* table for hashconsing *)
(* ********************* *)
let table = H.create 1289
(* ********************* *)
           
let make s =
  H.hashcons table s

let hash sym =
  let open Hashcons in
  sym.hkey

let compare s1 s2 =
  let open Hashcons in
  s1.tag - s2.tag

let equal x1 x2 =
  x1 == x2

let pp out at =
  let open Hashcons in
  Format.fprintf out "%s" at.node 

include Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
