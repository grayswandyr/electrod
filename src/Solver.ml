(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2024 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers

[@@@warning "-4"]
[@@@warning "-32"]

(* Computes the value of n according to the wrap around semantics *)
(* the result is betwwen -(2 ^ (bw-1)) and (2 ^ (bw-1) - 1) *)
let wrap bw n =
  if bw <= 0 then n
  else
    (* maxint = 2 ^ (bw - 1) => wraps at *this* number *)
    let maxint = Int.pow 2 (bw - 1) in
    let interval_length = 2 * maxint in
    if n >= maxint then ((n + maxint) mod interval_length) - maxint
    else if n < ~-maxint then
      ((n - maxint + 1) mod interval_length) + maxint - 1
    else n

(* fragile patterns, lots of them as we short-circuit *)

module type ATOMIC_PROPOSITION = sig
  type t

  val make : Domain.t -> Name.t -> Tuple.t -> t
  val compare : t -> t -> int
  val compare_string : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  (* None if non-enumerable; otw Some ar with ar >= 0  *)
  val domain_arity : t -> int option
  val is_const : t -> bool
  val is_partial : t -> bool
  val split_string : string -> (Name.t * Tuple.t) option
  val split : t -> (Name.t * Tuple.t) option
  val pp : Format.formatter -> t -> unit
end

module type LTL = sig
  module Atomic : ATOMIC_PROPOSITION

  type tcomp = Lte | Lt | Gte | Gt | Eq | Neq

  type t = private
    | Comp of tcomp * term * term
    | True
    | False
    | Atomic of Atomic.t
    | Auxiliary of Symbol.t
    | Not of t
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
    | Num of int
    | Neg of term
    | Bin of term * binop * term
    | AIte of t * term * term

  and binop = Plus | Minus | Mul | Div | Rem

  val true_ : t
  val false_ : t
  val atomic : Atomic.t -> t
  val auxiliary : Symbol.t -> t
  val not_ : t -> t
  val and_ : t -> t Lazy.t -> t
  val or_ : t -> t Lazy.t -> t
  val implies : t -> t Lazy.t -> t
  val xor : t -> t -> t
  val iff : t -> t -> t
  val conj : t list -> t
  val disj : t list -> t
  val wedge : range:'a Iter.t -> ('a -> t Lazy.t) -> t
  val vee : range:'a Iter.t -> ('a -> t Lazy.t) -> t
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
  val triggered : t -> t -> t
  val num : int -> int -> term
  val binary : term -> binop -> term -> term
  val plus : term -> term -> term
  val minus : term -> term -> term
  val neg : term -> term
  val mul : term -> term -> term
  val div : term -> term -> term
  val rem : term -> term -> term
  val ifthenelse_arith : t -> term -> term -> term
  val comp : tcomp -> term -> term -> t
  val lt : tcomp
  val lte : tcomp
  val gt : tcomp
  val gte : tcomp
  val eq : tcomp
  val neq : tcomp

  module Infix : sig
    (* precedence: from strongest to weakest *)
    (* 1 *)
    val ( !! ) : t -> t

    (* 2 *)
    val ( +|| ) : t -> t Lazy.t -> t
    val ( +&& ) : t -> t Lazy.t -> t

    (* 3 *)
    val ( @=> ) : t -> t Lazy.t -> t
    val ( @<=> ) : t -> t -> t
  end

  val stratify : smv_section:[ `Ltlspec | `Trans ] -> t -> t list
  val pp : Format.formatter -> int -> t -> unit

  val pp_gather_variables :
    ?next_is_X:bool ->
    int ->
    Symbol.t Iter.t ref ->
    Atomic.t Iter.t ref ->
    Format.formatter ->
    t ->
    unit
end

module LTL_from_Atomic (At : ATOMIC_PROPOSITION) : LTL with module Atomic = At =
struct
  module Atomic = At

  type tcomp = Lte | Lt | Gte | Gt | Eq | Neq

  (* let hash_tcomp = function *)
  (*   | Lte -> 3 *)
  (*   | Lt -> 5 *)
  (*   | Gte -> 7 *)
  (*   | Gt -> 11 *)
  (*   | Eq -> 13 *)
  (*   | Neq -> 17 *)

  type t =
    | Comp of tcomp * term * term
    | True
    | False
    | Atomic of Atomic.t
    | Auxiliary of Symbol.t
    | Not of t
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

  and term =
    | Num of int
    | Neg of term
    | Bin of term * binop * term
    | AIte of t * term * term

  and binop = Plus | Minus | Mul | Div | Rem

  (* default impl. for pp; to override later *)
  let stratify ~smv_section:_ f = [ f ]

  let pp _ _ =
    (* default impl. for pp; to override later *)
    failwith "Solver.LTL_from_Atomic.pp not implemented (on purpose)"

  let pp_gather_variables ?(next_is_X = true) _ _ =
    let _ = next_is_X in
    pp

  (* default impl. for pp; to override later *)

  (* let equal_tcomp_node x y = match x, y with  *)
  (*   | Lte, Lte *)
  (*   | Lt, Lt *)
  (*   | Gte, Gte *)
  (*   | Gt, Gt *)
  (*   | Eq, Eq  *)
  (*   | Neq, Neq -> true *)
  (*   | _ -> false *)

  let lt = Lt
  let lte = Lte
  let gt = Gt
  let gte = Gte
  let eq = Eq
  let neq = Neq
  let atomic at = Atomic at
  let auxiliary sym = Auxiliary sym
  let true_ = True
  let false_ = False

  let rec and_ p q =
    match (p, q) with
    | False, _ -> false_
    | True, (lazy q) -> q
    | Atomic at1, (lazy (Atomic at2)) when Atomic.equal at1 at2 -> p
    | _, (lazy q) -> (
        match q with False -> false_ | True -> p | _ -> And (p, q))

  and or_ p1 p2 =
    match (p1, p2) with
    | True, _ -> true_
    | False, (lazy p) -> p
    | Atomic at1, (lazy (Atomic at2)) when Atomic.equal at1 at2 -> p1
    | _, (lazy q) -> (
        match q with False -> p1 | True -> true_ | _ -> Or (p1, q))

  and not_ p =
    match p with
    | True -> false_
    | False -> true_
    (* | And (p, q) -> or_ (not_ p) (lazy (not_ q))
     * | Or (p, q) -> and_ (not_ p) (lazy (not_ q))
     * | Imp (p, q) -> and_ p (lazy (not_ q)) *)
    | Not q -> q
    | _ -> Not p

  and implies p q =
    match (p, q) with
    | False, _ -> true_
    | True, (lazy q2) -> q2
    | Atomic at1, (lazy (Atomic at2)) when Atomic.equal at1 at2 -> true_
    | _, (lazy q2) -> (
        match q2 with True -> true_ | False -> not_ p | _ -> Imp (p, q2))

  let xor p1 p2 = Xor (p1, p2)

  let iff p q =
    match (p, q) with
    | Atomic at1, Atomic at2 when Atomic.equal at1 at2 -> true_
    | False, False | True, True -> true_
    | False, True | True, False -> false_
    | _, _ -> Iff (p, q)

  let conj fmls = List.fold_left (fun a b -> and_ a (lazy b)) true_ fmls
  let disj fmls = List.fold_left (fun a b -> or_ a (lazy b)) false_ fmls

  let ifthenelse c t e =
    match c with True -> t | False -> e | _ -> Ite (c, t, e)

  let next p = X p
  let always p = G p
  let eventually p = F p
  let yesterday p = Y p
  let once p = O p
  let historically p = H p
  let until p1 p2 = U (p1, p2)
  let releases p1 p2 = R (p1, p2)
  let since p1 p2 = S (p1, p2)
  let triggered p1 p2 = T (p1, p2)

  let comp op t1 t2 =
    match (op, t1, t2) with
    | Eq, Num n, Num m when n = m -> true_
    | Lt, Num n, Num m when n < m -> true_
    | Lte, Num n, Num m when n <= m -> true_
    | Gt, Num n, Num m when n > m -> true_
    | Gte, Num n, Num m when n >= m -> true_
    | Neq, Num n, Num m when n <> m -> true_
    | (Lt | Lte | Gt | Gte | Neq), Num n, Num m when n = m -> false_
    | Eq, Num n, Num m when n <> m -> false_
    | _ -> Comp (op, t1, t2)

  (* OPTIMIZATIONS REMOVED *)
  (* let not_ p = Not p *)
  (* let and_ p (lazy q) = And (p, q) *)
  (* let or_ p (lazy q) = Or (p, q) *)
  (* let implies p (lazy q) = Imp (p, q) *)
  (* let iff p q = Iff (p, q) *)
  (* let plus t1 t2 = Plus (t1, t2) *)
  (* let minus t1 t2 = Minus (t1, t2) *)
  (* let neg t = Neg t *)
  (* let comp op t1 t2 = Comp (op, t1, t2) *)

  let num bw n = Num (wrap bw n)
  let binary t1 op t2 = Bin (t1, op, t2)

  let plus t1 t2 =
    match (t1, t2) with
    | Num 0, _ -> t2
    | _, Num 0 -> t1
    | _ -> Bin (t1, Plus, t2)

  let minus t1 t2 = match t2 with Num 0 -> t1 | _ -> Bin (t1, Minus, t2)
  let neg t = match t with Neg u -> u | _ -> Neg t

  let mul t1 t2 =
    match (t1, t2) with
    | Num 0, _ -> Num 0
    | _, Num 0 -> Num 0
    | _, _ -> Bin (t1, Mul, t2)

  let div t1 t2 =
    match (t1, t2) with Num 0, _ -> Num 0 | _, _ -> Bin (t1, Div, t2)

  let rem t1 t2 =
    match (t1, t2) with Num 0, _ -> Num 0 | _, _ -> Bin (t1, Rem, t2)

  let ifthenelse_arith c t e =
    match c with True -> t | False -> e | _ -> AIte (c, t, e)

  (* END term hashconsing *)

  let wedge ~range f =
    Iter.fold (fun fml tuple -> and_ fml @@ f tuple) true_ range

  let vee ~range f =
    Iter.fold (fun fml tuple -> or_ fml @@ f tuple) false_ range

  module Infix = struct
    (* precedence: from strongest to weakest *)
    (* 1 *)
    let ( !! ) x = not_ x

    (* 2 *)
    let ( +|| ) x y = or_ x y
    let ( +&& ) x y = and_ x y

    (* 3 *)
    let ( @=> ) x y = implies x y
    let ( @<=> ) x y = iff x y
  end
end

type script_type = Default of string | File of string

module type MODEL = sig
  type ltl
  type atomic

  type t = private {
    elo : Elo.t;
    init : (string * ltl) Iter.t;
    invariant : (string * ltl) Iter.t;
    trans : (string * ltl) Iter.t;
    property : string * ltl;
  }

  val make :
    elo:Elo.t ->
    init:(string * ltl) Iter.t ->
    invariant:(string * ltl) Iter.t ->
    trans:(string * ltl) Iter.t ->
    property:string * ltl ->
    t

  val analyze :
    conversion_time:Mtime.span ->
    cmd:string ->
    script:script_type ->
    keep_files:bool ->
    no_analysis:bool ->
    elo:Elo.t ->
    file:string ->
    bmc:int option ->
    pp_generated:bool ->
    t ->
    Outcome.t

  val pp : ?margin:int -> Format.formatter -> t -> unit
end
