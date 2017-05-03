

open Containers

type t = Atom.t Array.t

let of_list1 xs =
  assert (xs <> []);
  Array.of_list xs

let tuple1 at =
  Array.make 1 at

let arity tuple =
  Array.length tuple

let pp out atoms =
  let open Fmtc in
  (array ~sep:sp Atom.pp
   |> (if arity atoms > 1 then parens else (fun x -> x)))
    out atoms

let append (t1, t2) = Array.append t1 t2

let compare t1 t2 = Array.compare Atom.compare t1 t2

let equal t1 t2 = Array.equal Atom.equal t1 t2

module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 


module Set = CCSet.Make(struct
    type nonrec t = t
    let compare = compare
  end)
