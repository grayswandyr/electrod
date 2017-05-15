(** Abstraction of LTL w.r.t. any concrete implementation in a given
    model-checker.  *)

(** Abstract type for atomic propositions of LTL.  *)
module type ATOM =
  sig
    type t
    val make : Name.t -> Tuple.t -> t
    val pp : t Fmtc.t
    val to_string : t -> string
  end

(** Abstract type of LTL (contains past connectives as well as basic counting
    capabilities).  *)
module type S =
  sig
    type atom
    type tcomp = private Lte | Lt | Gte | Gt | Eq | Neq
    type t = private
        Comp of tcomp * term * term
      | True
      | False
      | Atom of atom
      | And of t * t
      | Or of t * t
      | Imp of t * t
      | Iff of t * t
      | Xor of t * t
      | Ite of t * t * t
      | X of t
      | F of t
      | G of t
      | Y of t
      | O of t
      | H of t
      | U of t * t
      | R of t * t
      | S of t * t
      | T of t * t
    and term = private
        Num of int
      | Plus of term * term
      | Minus of term * term
      | Neg of term
      | Count of t list
    val true_ : t
    val false_ : t
    val atom : Name.t -> Tuple.t -> t
    val not_ : t -> t
    val and_ : t -> t -> t
    val or_ : t -> t -> t
    val implies : t -> t -> t
    val xor : t -> t -> t
    val iff : t -> t -> t
    val conj : t list -> t
    val disj : t list -> t
    val wedge : range:'a Sequence.t -> ('a -> t) -> t
    val vee : range:'a Sequence.t -> ('a -> t) -> t
    val ifthenelse : t -> t -> t -> t
    val next : t -> t
    val always : t -> t
    val eventually : t -> t
    val yesterday : t -> t
    val once : t -> t
    val historically : t -> t
    val until : t -> t -> t
    val releases : t -> t -> t
    val since : t -> t -> t
    val trigerred : t -> t -> t
    val num : int -> term
    val plus : term -> term -> term
    val minus : term -> term -> term
    val neg : term -> term
    val count : t list -> term
    val comp : tcomp -> term -> term -> t
    val lt : tcomp
    val lte : tcomp
    val gt : tcomp
    val gte : tcomp
    val eq : tcomp
    val neq : tcomp
    module Infix :
      sig
        val ( !! ) : t -> t
        val ( +|| ) : t -> t -> t
        val ( +&& ) : t -> t -> t
        val ( @=> ) : t -> t -> t
        val ( @<=> ) : t -> t -> t
      end
  end

(** Builds an LTL implementation out of an implementation of atomic
    propositions. *)
module LTL_from_Atom :
  functor (At : ATOM) -> S with type atom = At.t


module type PrintableLTL = sig
  include S

  val pp : Format.formatter -> t -> unit
  include Intf.Print.S with type t := t
end
