open Containers

module type ATOM = sig
  type t

  val make : Name.t -> Tuple.t -> t

  val pp : Format.formatter -> t -> unit
  include Intf.Print.S with type t := t

end

module type S = sig
  type atom
    
  type tcomp = private
    | Lte 
    | Lt
    | Gte
    | Gt
    | Eq 
    | Neq

  type t = private
    | Comp of tcomp * term * term
    | True
    | False
    | Atom of atom
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

  val wedge : range:('a Sequence.t) -> ('a -> t) -> t
  val vee : range:('a Sequence.t) -> ('a -> t) -> t

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
    val ( +|| ) : t -> t -> t
    val ( +&& ) : t -> t -> t
    (* 3 *)
    val ( @=> ) : t -> t -> t
    val ( @<=> ) : t -> t -> t
  end

end


module LTL_from_Atom (At : ATOM) : S with type atom = At.t = struct
  type tcomp = 
    | Lte 
    | Lt
    | Gte
    | Gt
    | Eq 
    | Neq
  
  type atom = At.t

  and t =
    | Comp of tcomp * term * term
    | True
    | False
    | Atom of atom
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
    | Y of t                    (* yesterday *)
    | O of t                    (* once *)
    | H of t
    | U of t * t                (* until *)
    | R of t * t                (* releases *)
    | S of t * t                (* since *)
    | T of t * t                (* triggered *)  

  and term = 
    | Num of int 
    | Plus of term * term
    | Minus of term * term
    | Neg of term 
    | Count of t list

  let true_ = True
  let false_ = False

  let atom r ts = Atom (At.make r ts)

  let and_ p q = match p, q with
    | False, _
    | _, False -> False
    | True, _ -> q
    | _, True -> p
    | _, _ -> And (p, q)

  let or_ p1 p2 = match p1, p2 with
    | True, _
    | _, True -> True
    | False, p
    | p, False -> p
    | _, _ -> Or (p1, p2)
  
  let xor p1 p2 = Xor (p1, p2)

  let iff p q = match p, q with
    | False, False
    | True, True -> True
    | False, True
    | True, False -> False
    | _, _ -> Iff (p, q)

  let rec not_ = function
    | True -> False
    | False -> True
    | And (p, q) -> or_ (not_ p) (not_ q)
    | Or (p, q) -> and_ (not_ p) (not_ q)
    | Imp (p, q) -> and_ p (not_ q)
    | p -> Not p

  and implies p q = match p, q with
    | False, _ -> True
    | _, True -> True
    | True, _ -> q
    | _, False -> not_ p
    | _, _ -> Imp (p, q)

  let conj =
    List.fold_left and_ true_

  let disj =
    List.fold_left or_ false_

  let ifthenelse c t e = Ite (c, t, e)

  let next p = X p
  let always p = G p
  let eventually p = F p

  let yesterday p = Y p 
  let once p = O p
  let historically p = H p

  let until p1 p2 = U (p1, p2)
  let releases p1 p2 = R (p1, p2)
  let since p1 p2 = S (p1, p2)
  let trigerred p1 p2 = T (p1, p2)

  let num n = Num n
  let plus t1 t2 = match t1, t2 with
    | Num 0, _ -> t2
    | _, Num 0 -> t1
    | _ -> Plus (t1, t2)
             
  let minus t1 t2 = match t2 with
    | Num 0 -> t1
    | _ -> Minus (t1, t2)
             
  let neg t = match t with
    | Neg _ -> t
    | _ -> Neg t
             
  let count ps =
    match List.filter (function False -> false | _ -> true) ps with
      | [] -> num 0
      | props -> Count props

  let comp op t1 t2 = match op, t1, t2 with
    | Eq, Num n, Num m when n = m -> true_
    | (Lt | Lte | Gt | Gte | Neq), Num n, Num m when n = m -> false_
    | Eq, Num n, Num m when n <> m -> false_
    | _ -> Comp (op, t1, t2)
  let lt = Lt
  let lte = Lte
  let gt = Gt
  let gte = Gte
  let eq = Eq
  let neq = Neq

  

  (* OPTIMIZATIONS REMOVED *)
  let not_ p = Not p
  let and_ p q = And (p, q)
  let or_ p q = Or (p, q)
  let implies p q = Imp (p, q)
  (* let iff p q = Iff (p, q) *)
  (* let plus t1 t2 = Plus (t1, t2) *)
  (* let minus t1 t2 = Minus (t1, t2) *)
  (* let neg t = Neg t *)
  (* let comp op t1 t2 = Comp (op, t1, t2) *)
                        
  let wedge ~range f =
    Sequence.fold (fun fml tuple -> and_ fml @@ f tuple) true_ range

  let vee ~range f =
    Sequence.fold (fun fml tuple -> or_ fml @@ f tuple) false_ range

  module Infix = struct
    (* precedence: from strongest to weakest *)
    (* 1 *)
    let ( !! ) = not_
    (* 2 *)
    let ( +|| ) = or_
    let ( +&& ) = and_
    (* 3 *)
    let ( @=> ) = implies
    let ( @<=> ) = iff
  end
end

module type PrintableLTL = sig
  include S

  val pp : Format.formatter -> t -> unit
  include Intf.Print.S with type t := t
end
