

open Containers

type t = Atom.t Array.t

let of_list1 xs =
  assert (xs <> []);
  Array.of_list xs


let hash tuple =
  Hash.array Atom.hash tuple

let to_list = Array.to_list

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

let ( @@@ ) t1 t2 = Array.append t1 t2

let concat = function
  | [] -> invalid_arg "Tuple.concat: empty list of tuples"
  | hd::tl -> List.fold_left (@@@) hd tl

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

let pp out atoms =
  let open Fmtc in
  (array ~sep:sp Atom.pp
   |> (if arity atoms > 1 then parens else (fun x -> x)))
    out atoms
    
let join t1 t2 =
  let lg1 = Array.length t1 in
  let lg2 = Array.length t2 in
  assert (Atom.equal t1.(lg1 - 1) t2.(0));
  let res = Array.make (lg1 + lg2 - 2) t1.(0) in
  Array.blit t1 0 res 0 (lg1 - 1);
  Array.blit t2 1 res (lg1 - 1) (lg2 - 1);
  res

let split t len =
  let full_len = Array.length t in
  assert (len < full_len);
  let t1 = Array_slice.(make t 0 len |> copy) in
  let t2 = Array_slice.(make t len (full_len - len) |> copy) in
  (t1, t2)

let all_different tuple =
  let sorted = Array.sorted Atom.compare tuple in
  let lg = Array.length tuple in
  let i = ref 1 in
  let yes = ref true in
  while !yes && !i < lg do
    yes := Atom.compare sorted.(!i - 1) sorted.(!i) <> 0
  done;
  !yes

let to_1tuples t =
  Array.fold_right (fun at acc -> of_list1 [at] :: acc) t []

let to_ntuples n t =
  assert (Array.length t mod n = 0);
  Array.to_list t
  |> List.sublists_of_len n
  |> List.map Array.of_list


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 



(* module Set = CCSet.Make(struct *)
(*     type nonrec t = t *)
(*     let compare = compare *)
(*   end) *)

module Set = CCHashSet.Make(struct
    type nonrec t = t
    let equal = equal
    let hash = hash
  end)
