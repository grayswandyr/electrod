

open Containers

type t = Atom.t Array.t

let of_list1 xs =
  assert (xs <> []);
  Array.of_list xs

let tuple1 at =
  Array.make 1 at

let arity tuple =
  Array.length tuple

let transpose tuple =
  assert (arity tuple = 2);
  Array.rev tuple

let ith i tuple =
  assert (i >= 0 && i < arity tuple);
  Array.get tuple i

let ( @@@ ) (t1, t2) = Array.append t1 t2

let compare t1 t2 = Array.compare Atom.compare t1 t2

let equal t1 t2 = Array.equal Atom.equal t1 t2

let is_in_join tuple t1 t2 =
  let lg2 = Array.length t2 in
  let lg1 = Array.length t1 in
  let yes = ref @@ Atom.equal t1.(lg1 - 1) t2.(0) in
  let i = ref 0 in
  while !yes && !i < lg1 - 1 do
    yes := Atom.equal tuple.(!i) t1.(!i);
    incr i
  done;
  i := 1;
  while !yes && !i < lg2 - 1 do
    yes := Atom.equal tuple.(!i + lg1 - 1) t2.(!i);
    incr i
  done;
  !yes

let join t1 t2 =
  let lg1 = Array.length t1 in
  let lg2 = Array.length t2 in
  assert (t1.(lg1 - 1) = t2.(0));
  let res = Array.make (lg1 + lg2 - 2) t1.(0) in
  Array.blit t1 0 res 0 (lg1 - 1);
  Array.blit t2 1 res lg1 (lg2 - 1);
  res

let pp out atoms =
  let open Fmtc in
  (array ~sep:sp Atom.pp
   |> (if arity atoms > 1 then parens else (fun x -> x)))
    out atoms
    
module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 



module Set = CCSet.Make(struct
    type nonrec t = t
    let compare = compare
  end)
