
(** Provides fresh identifiers for variables (in formulas) at every stage. *)

(** type of an identifier: essentially a base string and a globally unique number *)
type t

(** Creates a fresh identifier. *)
val fresh : ?sep:string -> string -> t

val compare : t -> t -> int


include Intf.Print.S with type t := t
