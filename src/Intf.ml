(** Defines traits and mixins to use about anywhere. *)

module Print = struct
  module type S = sig
    type t

    val pp : t Fmtc.t

    val to_string : t -> string
  end

  module Mixin(M : sig type t val pp : t Fmtc.t end) : S with type t := M.t =
  struct
    include M
    
    let to_string = Fmtc.to_to_string pp
  end
end

    

module type COMPARE = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool
end
