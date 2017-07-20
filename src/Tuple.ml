

open Containers

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

let of_array contents = 
  {
    contents;
    hash = Hash.array Atom.hash contents
  }

let of_list1 xs =
  assert (xs <> []);
  let contents = Array.of_list xs in
  {
    contents;
    hash = Hash.array Atom.hash contents
  }


let hash tuple =
  tuple.hash

let to_list t = Array.to_list t.contents

let tuple1 at =
  of_list1 [at]


let transpose tuple =
  assert (arity tuple = 2);
  of_array @@ Array.rev tuple.contents

let ith i tuple =
  assert (i >= 0 && i < arity tuple);
  Array.get tuple.contents i

let ( @@@ ) t1 t2 =
  of_array @@ Array.append t1.contents t2.contents

let concat = function
  | [] -> invalid_arg "Tuple.concat: empty list of tuples"
  | hd::tl -> List.fold_left (@@@) hd tl

let compare t1 t2 = Array.compare Atom.compare t1.contents t2.contents

let equal t1 t2 = Array.equal Atom.equal t1.contents t2.contents


let is_in_join tup tuple1 tuple2 =
  let tuple = tup.contents in 
  let t1 = tuple1.contents in
  let t2 = tuple2.contents in
  let lg1 = Array.length t1 in
  let lg2 = Array.length t2 in
  (* convert in lists *)
  let ltup = Array.to_list tuple in
  let ltuple1 = Array_slice.(make t1 0 ~len:(lg1 - 1)
                             |> to_list) in
  let ltuple2 = Array_slice.(make t2 1 ~len:(lg2 - 1)
                             |> to_list) in
  List.equal Atom.equal ltup (ltuple1 @ ltuple2) 
  |> Fun.tap (fun res -> Msg.debug (fun m ->
        m "is_in_join: %a in %a.%a --> %B"
          pp tup
          pp tuple1
          pp tuple2
          res
      ))


  
    
let join tuple1 tuple2 =
  let t1 = tuple1.contents in
  let t2 = tuple2.contents in
  let lg1 = Array.length t1 in
  let lg2 = Array.length t2 in
  assert (Atom.equal t1.(lg1 - 1) t2.(0));
  let res = Array.make (lg1 + lg2 - 2) t1.(0) in
  Array.blit t1 0 res 0 (lg1 - 1);
  Array.blit t2 1 res (lg1 - 1) (lg2 - 1);
  of_array res

let split tuple len =
  let t = tuple.contents in
  let full_len = Array.length t in
  assert (len < full_len);
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



module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
 


module Set = CCSet.Make(struct
    type nonrec t = t
    let compare = compare
  end)
