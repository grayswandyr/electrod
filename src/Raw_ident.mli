(** Identifiers in "raw" ASTs. *)

(** Any form of identifier for constants (atoms, relations) in the source
    code. *)
type t = {
  ident : string;
  loc : Location.t
}

(** {1 Constructor}  *)

val ident : string -> Lexing.position -> Lexing.position -> t

(** {1 Accessors} *)
  
val basename : t -> string

val location : t -> Location.t

val eq_name : t -> t -> bool

include Intf.Print.S with type t := t
