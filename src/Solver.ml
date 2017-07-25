open Containers

module H = Hashcons

module type ATOMIC_PROPOSITION = sig
  type t

  val make : Name.t -> Tuple.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool

  val split : string -> Name.t * Tuple.t
  
  val pp : Format.formatter -> t -> unit
end

module type LTL = sig
  type atomic

  val make_atomic : Name.t -> Tuple.t -> atomic
  val split_atomic : string -> Name.t * Tuple.t
  val compare_atomic : atomic -> atomic -> int
    
  type tcomp = tcomp_node Hashcons_util.hash_consed

  and tcomp_node = private
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
    | Atomic of atomic
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

  val atomic : atomic -> t

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

  val pp_atomic : Format.formatter -> atomic -> unit

  val pp : Format.formatter -> t -> unit
end


module LTL_from_Atomic (At : ATOMIC_PROPOSITION) : LTL with type atomic = At.t = struct
  
  type atomic = At.t
                  
  let pp_atomic at = At.pp at 

  let equal_atomic at = At.equal at


  let make_atomic at = At.make at
  let compare_atomic at = At.compare at
  let split_atomic at = At.split at

  type tcomp =
    tcomp_node Hashcons_util.hash_consed
      [@equal fun a b -> equal_tcomp_node a.H.node b.H.node]

  and tcomp_node = 
    | Lte 
    | Lt
    | Gte
    | Gt
    | Eq 
    | Neq

  and t =
    t_node Hashcons_util.hash_consed

  and t_node = 
    | Comp of tcomp * term * term
    | True
    | False
    | Atomic of atomic
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
  module TComp = Hashcons.Make(struct
    type t = tcomp_node
    let hash x = Hashtbl.hash x
		let equal x y = match x, y with 
    | Lte, Lte
    | Lt, Lt
    | Gte, Gte
    | Gt, Gt
    | Eq, Eq 
    | Neq, Neq -> true
    | _ -> false
  end)

  let tcomp_table = TComp.create 29

  let make_tcomp x = TComp.hashcons tcomp_table x
  
  let lt = make_tcomp Lt
  let lte = make_tcomp Lte
  let gt = make_tcomp Gt
  let gte = make_tcomp Gte
  let eq = make_tcomp Eq
  let neq = make_tcomp Neq
  (* END tcomp hashconsing *)


  (* BEGIN t hashconsing *)
  module T = Hashcons.Make(struct
    type t = t_node
    let hash x = Hashtbl.hash x
		let equal x y = failwith "equal t TODO"
  end)

  let t_table = T.create 297

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

  let comp op t1 t2 = match op.H.node, t1.H.node, t2.H.node with
    | Eq, Num n, Num m when n = m -> true_
    | Lt, Num n, Num m when n < m -> true_
    | Lte, Num n, Num m when n <= m -> true_
    | Gt, Num n, Num m when n > m -> true_
    | Gte, Num n, Num m when n >= m -> true_
    | Neq, Num n, Num m when n <> m -> true_
    | (Lt | Lte | Gt | Gte | Neq), Num n, Num m when n = m -> false_
    | Eq, Num n, Num m when n <> m -> false_
    | _ -> make_t @@ Comp (op, t1, t2)
  (* END t hashconsing *)

  (* BEGIN term hashconsing *)
  module Term = Hashcons.Make(struct
    type t = term_node
    let hash x = Hashtbl.hash x
		let equal x y = match x, y with
      | Num n, Num m -> n = m 
      | Plus (n1, m1), Plus (n2, m2) -> n1 == n2 && m1 == m2
      | Minus (n1, m1), Minus (n2, m2) -> n1 == n2 && m1 == m2
      | Neg n, Neg m -> n == m
      | Count ps, Count qs ->
          List.fold_left2 (fun acc p q -> p == q && acc) true ps qs
      | _ -> false
  end)

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
