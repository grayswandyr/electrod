open Containers
      
type t = ((Name.t * Tuple.t) list) * ((Name.t * Tuple.t) list)

                                       
let make x y = (x, y)                                       

let fold f (sym : t ) acc =
  let (l1, l2) = sym in
  List.fold_right2 f l1 l2 acc 
                 
