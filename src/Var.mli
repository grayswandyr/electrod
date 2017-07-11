
(** Provides fresh identifiers for variables (in formulas) at every stage. *)

(** type of an identifier: essentially a base string and a globally unique number *)
type t

(** Creates a fresh identifier ([loc] is the location of the originating
    identifier, if there is one). *)
val fresh : ?sep:string -> ?loc:Location.t -> string -> t

val fresh_copy : t  -> t

val fresh_of_raw_ident : ?sep:string -> Raw_ident.t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val style : Fmt.style


include Intf.Print.S with type t := t
