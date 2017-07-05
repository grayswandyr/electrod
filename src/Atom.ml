
open Containers

module Basis = struct
  type t = {
    name : string;
    loc : Location.t option
  }

  let compare at1 at2 = CCString.compare at1.name at2.name

  let equal at1 at2 = CCString.equal at1.name at2.name
end

include Basis

let atom ?loc name = { name; loc }

let of_raw_ident id = {
  name = Raw_ident.basename id;
  loc = Some (Raw_ident.location id)
}


let hash atom = Hash.string atom.name

(** Generic interface implementations *)

let pp out { name; _ } =
  Fmtc.(string out name)

module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P


let pp_list atoms =
  Fmtc.(braces_ @@ list ~sep:sp pp) atoms

let to_string_list = Fmtc.to_to_string pp_list


module Set = Set.Make(Basis)
