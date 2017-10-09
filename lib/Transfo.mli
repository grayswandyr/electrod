(** Specification of transformations. *)

(** A transformation conforms to the following signature.  *)
type ('src, 'dst) t

(** Constructor. The first parameter is the name: it must be unique in the whole
    program.*)
val make : string -> ('src -> 'dst) -> ('src, 'dst) t

(** Retrieves the name... *)
val name : ('src, 'dst) t -> string

(** Runs a transformation *)
val run : ('src, 'dst) t -> 'src -> 'dst

(** Returns the composition (left-to-right) of two transformations. *)
val fby : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

(** Identity transformation *)
val identity : ('a, 'a) t 

(** A list of transformations with given source and target type. *)
type ('src, 'dst) tlist

(** Creates a list of transformations with a default one .*)
val tlist : ('src, 'dst) t list -> ('src, 'dst) tlist

(** Retrieves a transformation. May raise Not_found. *)
val get_exn : ('src, 'dst) tlist -> string -> ('src, 'dst) t
