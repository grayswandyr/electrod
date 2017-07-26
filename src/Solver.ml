open Containers

module H = Hashcons

module type ATOMIC_PROPOSITION = sig
  type t

  val make : Name.t -> Tuple.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash  : t -> int

  val split : string -> (Name.t * Tuple.t) option
  
  val pp : Format.formatter -> t -> unit
end

module type LTL = sig
  module Atomic : ATOMIC_PROPOSITION
    
  type tcomp = 
    | Lte 
    | Lt
    | Gte
    | Gt
    | Eq 
    | Neq

  type t = t_node Hashcons_util.hash_consed

  and t_node = private
    | Comp of tcomp * term * term
    | True
    | False
    | Atomic of Atomic.t
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

  and term = term_node Hashcons_util.hash_consed

  and term_node = private
    | Num of int 
    | Plus of term * term
    | Minus of term * term
    | Neg of term 
    | Count of t list

  val true_ : t
  val false_ : t

  val atomic : Atomic.t -> t

  val not_ : t -> t

  val and_ : t -> t Lazy.t -> t
  val or_ : t -> t Lazy.t -> t
  val implies : t -> t Lazy.t -> t
  val xor : t -> t -> t
  val iff : t -> t -> t

  val conj : t list -> t
  val disj : t list -> t

  val wedge : range:('a Sequence.t) -> ('a -> t Lazy.t) -> t
  val vee : range:('a Sequence.t) -> ('a -> t Lazy.t) -> t

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

  val pp : Format.formatter -> t -> unit

  val pp_hasconsing_assessment :
           Format.formatter ->
           (Format.formatter -> t -> unit) -> unit
end


module LTL_from_Atomic (At : ATOMIC_PROPOSITION) : LTL with module Atomic = At = struct
  module Atomic = At

  type tcomp =
    | Lte 
    | Lt 
    | Gte 
    | Gt 
    | Eq 
    | Neq 
  [@@deriving show]

  let hash_tcomp = function
    | Lte -> 3
    | Lt -> 5
    | Gte -> 7
    | Gt -> 11
    | Eq -> 13
    | Neq -> 17

  type t =
    t_node Hashcons_util.hash_consed
      

  and t_node = 
    | Comp of tcomp * term * term
    | True
    | False
    | Atomic of Atomic.t 
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
    term_node Hashcons_util.hash_consed
      

  and term_node = 
    | Num of int 
    | Plus of term * term 
    | Minus of term * term 
    | Neg of term 
    | Count of t list
  [@@deriving show]             (* default impl. for pp; to override later *)

  (* BEGIN tcomp hashconsing *)
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
  (* END tcomp hashconsing *)


  (* BEGIN t hashconsing *)

  module T_node = struct 
    type t = t_node

		let equal (t1 : t) (t2 : t) = match t1, t2 with
        | True, True 
        | False, False -> true
        | Atomic a1, Atomic a2 -> Atomic.equal a1 a2
        | Not a1, Not a2 -> a1 == a2
        | X a1, X a2 -> a1 == a2
        | F a1, F a2 -> a1 == a2
        | G a1, G a2 -> a1 == a2
        | Y a1, Y a2 -> a1 == a2
        | O a1, O a2 -> a1 == a2
        | H a1, H a2 -> a1 == a2
        | U (a1, b1), U (a2, b2) -> a1 == a2 && b1 == b2
        | R (a1, b1), R (a2, b2) -> a1 == a2 && b1 == b2
        | S (a1, b1), S (a2, b2) -> a1 == a2 && b1 == b2
        | T (a1, b1), T (a2, b2) -> a1 == a2 && b1 == b2
        | And (a1, b1), And (a2, b2) -> a1 == a2 && b1 == b2
        | Or (a1, b1), Or (a2, b2) -> a1 == a2 && b1 == b2
        | Imp (a1, b1), Imp (a2, b2) -> a1 == a2 && b1 == b2
        | Iff (a1, b1), Iff (a2, b2) -> a1 == a2 && b1 == b2
        | Xor (a1, b1), Xor (a2, b2) -> a1 == a2 && b1 == b2
        | Ite (a1, b1, c1), Ite (a2, b2, c2) -> a1 == a2 && b1 == b2 && c1 == c2
        | Comp (a1, b1, c1), Comp (a2, b2, c2) -> a1 == a2 && b1 == b2 && c1 == c2
        | _ -> false
    
    let hash (tn : t_node) =
      let open Hashcons in
      match tn with
        | True -> 19
        | False -> 23
        | Atomic a -> Hash.combine2 29 @@ Atomic.hash a
        | Not a -> Hash.combine2 31 a.hkey
        | X a -> Hash.combine2 37 a.hkey
        | F a -> Hash.combine2 41 a.hkey
        | G a -> Hash.combine2 43 a.hkey
        | Y a -> Hash.combine2 47 a.hkey
        | O a -> Hash.combine2 53 a.hkey
        | H a -> Hash.combine2 59 a.hkey
        | U (a, b) -> Hash.combine3 61 a.hkey b.hkey
        | R (a, b) -> Hash.combine3 67 a.hkey b.hkey
        | S (a, b) -> Hash.combine3 71 a.hkey b.hkey
        | T (a, b) -> Hash.combine3 73 a.hkey b.hkey
        | And (a, b) -> Hash.combine3 79 a.hkey b.hkey
        | Or (a, b) -> Hash.combine3 83 a.hkey b.hkey
        | Imp (a, b) -> Hash.combine3 89 a.hkey b.hkey
        | Iff (a, b) -> Hash.combine3 97 a.hkey b.hkey
        | Xor (a, b) -> Hash.combine3 101 a.hkey b.hkey
        | Ite (a, b, c) -> Hash.combine4 103 a.hkey b.hkey c.hkey
        | Comp (a, b, c) -> Hash.combine4 107 (hash_tcomp a) b.hkey c.hkey

  end
  
  module T = Hashcons.Make(T_node)

  let t_table = T.create 2971

  let make_t x = T.hashcons t_table x
  
  let atomic at = make_t @@ Atomic at
  let true_ = make_t @@ True
  let false_ = make_t @@ False

  let and_ p q = match p.H.node, q with
    | False, _ -> false_
    | True, lazy q -> q
    | _, lazy q ->
        match q.H.node with
          | False -> false_
          | True -> p
          | _ -> make_t @@ And (p, q)

  let or_ p1 p2 = match p1.H.node, p2 with
    | True, _ -> true_
    | False, lazy p -> p
    | p, lazy q ->
        match q.H.node with
          | False -> p1
          | True -> true_
          | _ -> make_t @@ Or (p1, q)
  
  let xor p1 p2 = make_t @@ Xor (p1, p2)

  let iff p q = match p.H.node, q.H.node with
    | False, False
    | True, True -> true_
    | False, True
    | True, False -> false_
    | _, _ -> make_t @@ Iff (p, q)

  let rec not_ p = match p.H.node with
    | True -> false_
    | False -> true_ 
    | And (p, q) -> or_ (not_ p) (lazy (not_ q))
    | Or (p, q) -> and_ (not_ p) (lazy (not_ q))
    | Imp (p, q) -> and_ p (lazy (not_ q))
    | Not q -> q
    | _ -> make_t @@ Not p

  and implies p q = match p.H.node, q with
    | False, _ -> true_
    | True, lazy q2 -> q2
    | _, lazy q2 ->
        match q2.H.node with
          | True -> true_
          | False -> not_ p
          | _ -> make_t @@ Imp (p, q2)

  let conj fmls =
    List.fold_left (fun a b -> and_ a (lazy b)) true_ fmls

  let disj fmls =
    List.fold_left (fun a b -> or_ a (lazy b)) false_ fmls

  let ifthenelse c t e = match c.H.node with
    | True -> t
    | False -> e
    | _ -> make_t @@ Ite (c, t, e)

  let next p = make_t @@ X p
  let always p = make_t @@ G p
  let eventually p = make_t @@ F p

  let yesterday p = make_t @@ Y p 
  let once p = make_t @@ O p
  let historically p = make_t @@ H p

  let until p1 p2 = make_t @@ U (p1, p2)
  let releases p1 p2 = make_t @@ R (p1, p2)
  let since p1 p2 = make_t @@ S (p1, p2)
  let trigerred p1 p2 = make_t @@ T (p1, p2)

  let comp op t1 t2 = match op, t1.H.node, t2.H.node with
    | Eq, Num n, Num m when n = m -> true_
    | Lt, Num n, Num m when n < m -> true_
    | Lte, Num n, Num m when n <= m -> true_
    | Gt, Num n, Num m when n > m -> true_
    | Gte, Num n, Num m when n >= m -> true_
    | Neq, Num n, Num m when n <> m -> true_
    | (Lt | Lte | Gt | Gte | Neq), Num n, Num m when n = m -> false_
    | Eq, Num n, Num m when n <> m -> false_
    | _ -> make_t @@ Comp (op, t1, t2)

  

  (* OPTIMIZATIONS REMOVED *)
  (* let not_ p = make_t @@ Not p *)
  (* let and_ p (lazy q) = make_t @@ And (p, q) *)
  (* let or_ p (lazy q) = make_t @@ Or (p, q) *)
  (* let implies p (lazy q) = make_t @@ Imp (p, q) *)
  (* let iff p q = make_t @@ Iff (p, q) *)
  (* let plus t1 t2 = make_t @@ Plus (t1, t2) *)
  (* let minus t1 t2 = make_t @@ Minus (t1, t2) *)
  (* let neg t = make_t @@ Neg t *)
  (* let comp op t1 t2 = make_t @@ Comp (op, t1, t2) *)
                       
  (* END t hashconsing *)

  (* BEGIN term hashconsing *)

  module Term_node = struct
    type t = term_node
      
		let equal x y = match x, y with
      | Num n, Num m -> n == m
      | Plus (n1, m1), Plus (n2, m2) -> n1 == n2 && m1 == m2
      | Minus (n1, m1), Minus (n2, m2) -> n1 == n2 && m1 == m2
      | Neg n, Neg m -> n == m
      | Count ps, Count qs ->
          List.fold_left2 (fun acc p q -> p == q && acc) true ps qs
      | _ -> false

    let hash (tn : term_node) =
      let open Hashcons in
      match tn with
        | Num n -> Hash.combine2 109 @@ Hash.int n
        | Neg a -> Hash.combine2 113 a.hkey
        | Plus (a, b) -> Hash.combine3 127 a.hkey b.hkey
        | Minus (a, b) -> Hash.combine3 131 a.hkey b.hkey
        | Count ps -> Hash.list (fun p -> p.hkey) ps
  end
  
  module Term = Hashcons.Make(Term_node)

  let term_table = Term.create 29

  let make_term x = Term.hashcons term_table x
                    
  let num n = make_term @@ Num n
                          
  let plus t1 t2 = match t1.H.node, t2.H.node with
    | Num 0, _ -> t2
    | _, Num 0 -> t1
    | _ -> make_term @@ Plus (t1, t2)
             
  let minus t1 t2 = match t2.H.node with
    | Num 0 -> t1
    | _ -> make_term @@ Minus (t1, t2)
             
  let neg t = match t.H.node with
    | Neg _ -> t
    | _ -> make_term @@ Neg t
             
  let count ps =
    match List.filter (fun p -> p.H.node != False) ps with
      | [] -> num 0
      | props -> make_term @@ Count props
                                
  (* END term hashconsing *)
                                
                        
  let wedge ~range f =
    Sequence.fold (fun fml tuple -> and_ fml @@ f tuple) true_ range

  let vee ~range f =
    Sequence.fold (fun fml tuple -> or_ fml @@ f tuple) false_ range

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
  

  let pp_hasconsing_assessment out pp_t =
    let open Fmtc in
    let pr_stats fstats out hctable =
      let table_length, number_of_entries, sum_of_bucket_lengths,
          smallest_bucket_length, median_bucket_length, biggest_bucket_length =
        fstats hctable
      in
      pf out "table_length = %d@\n\
              number_of_entries = %d@\n\
              sum_of_bucket_lengths = %d@\n\
              smallest_bucket_length = %d@\n\
              median_bucket_length = %d@\n\
              biggest_bucket_length = %d@\n"
        table_length number_of_entries sum_of_bucket_lengths
        smallest_bucket_length median_bucket_length biggest_bucket_length
    in
    pf out "LTL hashconsing assessment@\n\
            [LTL]@\n@[<v>%a@]\
            [TERMS]@\n@[<v>%a@]\
            LTL table:@\n"
      (pr_stats T.stats) t_table
      (pr_stats Term.stats) term_table(* ; *)
    (* Format.pp_open_vbox out 0; *)
    (* T.iter (pp_t out) t_table; *)
    (* Format.pp_close_box out () *)
end

type outcome =
  | No_trace
  | Trace of Trace.t

let pp_outcome out =
  let open Fmtc in
  function
    | No_trace -> pf out "--no trace--"
    | Trace t -> pf out "@[<v>%a@]" Trace.pp t
    
type script_type =
  | Default of string
  | File of string
      
module type MODEL = sig 
  type ltl

  type atomic

  type t = private {
    rigid : atomic Sequence.t;
    flexible : atomic Sequence.t;    
    invariant : ltl Sequence.t;
    property : ltl 
  }

  val make :
    rigid:atomic Sequence.t
    -> flexible:atomic Sequence.t
    -> invariant:ltl Sequence.t 
    -> property:ltl -> t

  val analyze : cmd:string 
    -> script:script_type
    -> keep_files:bool
    -> elo:Elo.t
    -> file:string -> t -> outcome

  val pp : ?margin:int -> Format.formatter -> t -> unit
end
