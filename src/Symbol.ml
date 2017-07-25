open Containers

(* Hashcons_util stands for Hashcons, see Hashcons_util.ml *)
type t = string Hashcons_util.hash_consed

module H = Hashcons.Make(struct
    type t = string
    let hash = String.hash
		let equal = String.equal 
  end)
  
(* ********************* *)
(* table for hashconsing *)
(* ********************* *)
let table = H.create 297
(* ********************* *)
           
let make s =
  H.hashcons table s

let hash sym =
  let open Hashcons in
  sym.tag

let compare s1 s2 =
  let open Hashcons in
  Int.compare s1.tag s2.tag

let equal x1 x2 =
  let open Hashcons in
  x1.tag = x2.tag

let pp out at =
  let open Hashcons in
  Format.fprintf out "%s" at.node 

include Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
