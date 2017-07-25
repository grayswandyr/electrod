(** Symbols are hash-consed strings *)

open Containers

type t

val make : string -> t

val compare : t -> t -> int

val hash : t -> int

val equal : t -> t -> bool
  

include Intf.Print.S with type t := t
