
type t = {
  ident : string;
  loc : Location.t
}

let ident ident begp endp =
  let loc = Location.from_positions begp endp in
  { ident; loc }

let basename { ident; _ } = ident

let location { loc; _ } = loc

let eq_name i1 i2 = i1.ident = i2.ident

let pp out { ident; _} =
  Fmtc.string out ident


 
