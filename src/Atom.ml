
open Containers

type t = {
  sym : Symbol.t;
  loc : Location.t option
}

let compare a1 a2 = Symbol.compare a1.sym a2.sym

let equal a1 a2 = Symbol.equal a1.sym a2.sym

let atom ?loc s = { sym = Symbol.make s; loc }

let of_raw_ident id =
  atom ~loc:(Raw_ident.location id) (Raw_ident.basename id)
 

let hash atom = Symbol.hash atom.sym

(** Generic interface implementations *)

let pp out { sym; _ } =
  Symbol.pp out sym



module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
 



let pp_list atoms =
  Fmtc.(braces_ @@ list ~sep:sp pp) atoms

let to_string_list = Fmtc.to_to_string pp_list


