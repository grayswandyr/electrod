(** Represents SMV files and how to produce them *)

(** Given an implementation for atoms, provides a LTL implementation with a
    pretty printing function for LTL formulas.  *)
module MakePrintableLTL :
  functor (At : LTL.ATOM) -> LTL.PrintableLTL with type atom = At.t

(** TODO: implement abstract file format functions  *)
module MakeFile : functor (Ltl : LTL.PrintableLTL)
  -> LTL.PRINTABLE_MODEL with type ltl = Ltl.t and type atom = Ltl.atom
