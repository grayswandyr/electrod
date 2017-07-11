open Containers
    
type t = string


let name s = s

let dummy =
  let c = ref 0 in
  fun () ->
    "dummy!" ^ (string_of_int @@ CCRef.get_then_incr c)

let of_raw_ident id = Raw_ident.basename id

let univ = "univ"

let iden = "iden"

let equal = String.equal

let style = `Cyan

let pp out name =
  Fmtc.(styled style string) out name


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 

module Map = CCMap.Make(struct type t = string let compare = CCOrd.string end)
