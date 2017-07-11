open Containers
      
type t = ((Name.t * Tuple.t) list) * ((Name.t * Tuple.t) list)

let make x y = (x, y)                                       
