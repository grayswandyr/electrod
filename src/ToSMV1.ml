open Containers


module Logic = struct
  module Atom = struct
    type t = string

    let pp out at =
      Format.fprintf out "%s" at

    let make name atoms =
      Format.sprintf "%a_%a" Name.pp name Tuple.pp atoms


    module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
    include P 
  end

  module Ltl = SMV.MakeLTL(Atom)
  include Ltl
end
