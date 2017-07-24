(*${*) open Containers (*$}*)

(*$inject open Test *)

type t = {
  contents : Atom.t Array.t;
  hash : int
}


let arity tuple =
  Array.length tuple.contents

let pp out atoms =
  let open Fmtc in
  (array ~sep:sp Atom.pp
   |> (if arity atoms > 1 then parens else (fun x -> x)))
    out atoms.contents


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P  


let of_array contents =
  assert (Array.length contents > 0);
  {
    contents;
    hash = Hash.array Atom.hash contents
  }

let of_list1 xs =
  assert (xs <> []);
  of_array @@ Array.of_list xs 


let hash tuple =
  tuple.hash

let to_list t = Array.to_list t.contents

let tuple1 at =
  of_list1 [at]
    
let compare t1 t2 = Array.compare Atom.compare t1.contents t2.contents

let equal t1 t2 = Array.equal Atom.equal t1.contents t2.contents


(* transpose involutive *)
(*$Q transpose
  any_tuple2 (fun tuple -> \
  transpose (transpose tuple) = tuple)
*)
let transpose tuple =
  assert (arity tuple = 2);
  of_array @@ Array.rev tuple.contents
                

let ith i tuple =
  assert (i >= 0 && i < arity tuple);
  tuple.contents.(i)

let ( @@@ ) t1 t2 =
  of_array @@ Array.append t1.contents t2.contents

let concat = function
  | [] -> invalid_arg "Tuple.concat: empty list of tuples"
  | hd::tl -> List.fold_left (@@@) hd tl

  

(* join iden right neutral *)
(*$Q join
  Q.(pair any_tuple any_tuple2) (fun (t1, iden) -> \
  Q.assume (Atom.equal (ith (arity t1 - 1) t1) (ith 0 iden));\
  Q.assume (Atom.equal (ith 0 iden) (ith 1 iden));\
  equal (join t1 iden) t1 \
  )
*)
(* join iden left neutral *)
(*$Q join
  Q.(pair any_tuple2 any_tuple) (fun (iden, t1) -> \
  Q.assume (Atom.equal (ith 1 iden) (ith 0 t1));\
  Q.assume (Atom.equal (ith 0 iden) (ith 1 iden));\
  equal (join iden t1) t1 \
  )
*)
let join tuple1 tuple2 =
  let t1 = tuple1.contents in
  let t2 = tuple2.contents in
  let lg1 = Array.length t1 in
  let lg2 = Array.length t2 in
  assert (Atom.equal (ith (arity tuple1 - 1) tuple1) (ith 0 tuple2));
  let res = Array.make (lg1 + lg2 - 2) t1.(0) in
  Array.blit t1 0 res 0 (lg1 - 1);
  Array.blit t2 1 res (lg1 - 1) (lg2 - 1);
  of_array res



let is_in_join tup tuple1 tuple2 =
  let t1 = tuple1.contents in
  let t2 = tuple2.contents in
  let lg1 = Array.length t1 in
  Atom.equal t1.(lg1 - 1) t2.(0) &&
  equal tup @@ join tuple1 tuple2
  (* |> Fun.tap (fun res -> Msg.debug (fun m -> *)
  (*       m "is_in_join: %a in %a.%a --> %B" *)
  (*         pp tup *)
  (*         pp tuple1 *)
  (*         pp tuple2 *)
  (*         res)) *)

(* split "inverse" to (@@@) *)
(*$Q split
  Q.(pair any_tuple (int_range 1 9)) (fun (tuple, len) -> \
  Q.assume (len > 0 && len < arity tuple);\
  equal tuple (Fun.uncurry (@@@) @@ split tuple len)\
  )
*)
(* other direction *)
(*$Q split
  Q.(pair any_tuple any_tuple) (fun (t1, t2) -> \
  let t = (t1 @@@ t2) in \
  let l1 = arity t1 in \
  let (a, b) = (split t l1) in \
  equal a t1 && equal b t2 \
  )
*)
let split tuple len =
  let t = tuple.contents in
  let full_len = Array.length t in
  assert (len > 0 && len < full_len);
  let t1 = Array_slice.(make t 0 len |> copy) in
  let t2 = Array_slice.(make t len (full_len - len) |> copy) in
  (of_array t1, of_array t2)



let all_different tuple =
  let t = tuple.contents in
  let sorted = Array.sorted Atom.compare t in
  let lg = Array.length t in
  let i = ref 1 in
  let yes = ref true in
  while !yes && !i < lg do
    yes := Atom.compare sorted.(!i - 1) sorted.(!i) <> 0
  done;
  !yes

let to_1tuples t =
  Array.fold_right (fun at acc -> of_list1 [at] :: acc) t.contents []

let to_ntuples n t =
  assert (Array.length t.contents mod n = 0);
  Array.to_list t.contents
  |> List.sublists_of_len n
  |> List.map of_list1




module Set = CCSet.Make(struct
    type nonrec t = t
    let compare = compare
  end)
