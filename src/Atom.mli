(** Atoms (= urelements). *)

(** Type of atoms. *)
type t 

(** [atom ~loc:loc s] creates an atom with name [s] and optional location [loc]. *)
val atom : ?loc:Location.t -> string -> t

(** creates an atom out of a raw_ident. *)
val of_raw_ident : Raw_ident.t -> t

(** Prints a list of atoms as a bound. *)
val pp_list : t list CCFormat.printer

val hash : t -> int
  

include Intf.Print.S with type t := t
  
include Intf.COMPARE with type t := t

