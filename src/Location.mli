(** Locations in a file (issued from the parsing phase). *)

(** A location in a file represents an interval from a start position to an end one. *)
type t 

(** [from_positions begp endp] takes {!Lexing.position}s [begp] and [endp]
    position returns a location out of them. Requires: [begp] < [endp]. *)
val from_positions : Lexing.position -> Lexing.position -> t

(** Accessors: *)

val begl : t -> int
val begc : t -> int
val endl : t -> int
val endc : t -> int

(** Merge two positions *)
val span : t * t -> t

(** Dummy position *)
val dummy : t

type 'a located = {
  data : 'a;
  loc : t;
}[@@deriving make]

val pp_located : (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a located -> unit



include Intf.Print.S with type t := t
