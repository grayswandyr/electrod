open Containers
      
type t = ((Name.t * Tuple.t) list) * ((Name.t * Tuple.t) list)

                                       
let make x y = (x, y)                                       

let fold f acc (sym : t ) =
  let (l1, l2) = sym in
  List.fold_left2 f acc l1 l2 
                 
