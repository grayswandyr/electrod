open Containers

(* to allow deriving show, we must make this special redefinition *)
module M = struct
  type +'a hash_consed = 'a Hashcons.hash_consed = private {
    hkey : int;
    tag : int;
    node : 'a }
  [@@deriving show]
end;;

type t = string M.hash_consed

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

let hash sym = sym.M.tag

let compare s1 s2 =
  let open Hashcons in
  Int.compare s1.tag s2.tag

let equal x1 x2 = x1.M.tag = x2.M.tag

let pp out at =
  let open Hashcons in
  Format.fprintf out "%s" at.node 

include Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
