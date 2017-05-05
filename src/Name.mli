(** Names (of relations for instance). *)

type t 

(** {1 Constructors} *)
val name : string -> t

(** Returns a dummy name. This function is only here for some computations that
    raise new relations and hence need a temporary, dummy name. *)
val dummy : unit -> t

val of_raw_ident : Raw_ident.t -> t

(** Reserved name for 'univ' and 'iden'. *)
val univ : t
val iden : t

(** Tells whether two names are the same *)
val equal : t -> t -> bool

val style : Fmt.style

include Intf.Print.S with type t := t

module Map : CCMap.S with type key = t
