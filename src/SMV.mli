(** Represents SMV files and how to produce them *)

(** Given an implementation for atoms, provides a LTL implementation with a
    pretty printing function for LTL formulas.  *)
module MakePrintableLTL :
  functor (At : LTL.ATOM) -> LTL.PrintableLTL 

(** TODO: implement abstract file format functions  *)
module File : functor (Logic : LTL.PrintableLTL) -> sig  end
