open Containers


module Logic = struct
  module Atom = struct
    type t = string

    let pp out at =
      Format.fprintf out "%s" at

    let make name atoms =
      let ats = Tuple.to_list atoms in
      Format.sprintf "%a_%a" Name.pp name Fmtc.(list ~sep:underscore Atom.pp) ats


    module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
    include P 
  end

  module Ltl = SMV.MakeLTL(Atom)
  include Ltl
end
