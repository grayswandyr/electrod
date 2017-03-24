(** A generic type for goals (used in {!Raw} and {!Elo}) *)

type 'a located = { data : 'a; loc : Location.t; }
type ('v, 'i) t = Sat of ('v, 'i) fml list
and ('v, 'i) fml = ('v, 'i) prim_fml located
and ('v, 'i) prim_fml = private
    FBuiltin of string * ('v, 'i) exp list
  | True
  | False
  | Qual of rqualify * ('v, 'i) exp
  | RComp of ('v, 'i) exp * comp_op * ('v, 'i) exp
  | IComp of ('v, 'i) iexp * icomp_op * ('v, 'i) iexp
  | LUn of lunop * ('v, 'i) fml
  | LBin of ('v, 'i) fml * lbinop * ('v, 'i) fml
  | QAEN of ae_quant * ('v, 'i) sim_binding list * ('v, 'i) block
  | QLO of lo_quant * ('v, 'i) binding list * ('v, 'i) block
  | Let of ('v, 'i) binding list * ('v, 'i) block
  | FIte of ('v, 'i) fml * ('v, 'i) fml * ('v, 'i) fml
  | Block of ('v, 'i) block
and ('v, 'i) binding = 'v * ('v, 'i) exp
and ('v, 'i) sim_binding = disj * 'v list * ('v, 'i) exp
and disj = bool
and ('v, 'i) block = ('v, 'i) fml list
and ae_quant = private All | Some_ | No
and lo_quant = private One | Lone
and lbinop = private And | Or | Imp | Iff | U | R | S
and lunop = private F | G | Not | O | X | H | P
and comp_op = private In | NotIn | REq | RNEq
and icomp_op = private IEq | INEq | Lt | Lte | Gt | Gte
and ('v, 'i) exp = ('v, 'i) prim_exp located
and ('v, 'i) prim_exp = private
    None_
  | Univ
  | Iden
  | Ident of 'i
  | RUn of runop * ('v, 'i) exp
  | RBin of ('v, 'i) exp * rbinop * ('v, 'i) exp
  | RIte of ('v, 'i) fml * ('v, 'i) exp * ('v, 'i) exp
  | BoxJoin of ('v, 'i) exp * ('v, 'i) exp list
  | Compr of ('v, 'i) sim_binding list * ('v, 'i) block
  | Prime of ('v, 'i) exp
and rqualify = private ROne | RLone | RSome | RNo
and runop = private Transpose | TClos | RTClos
and rbinop = private Union | Inter | Over | LProj | RProj | Prod | Diff | Join
and ('v, 'i) iexp = ('v, 'i) prim_iexp located
and ('v, 'i) prim_iexp = private
    Num of int
  | Card of ('v, 'i) exp
  | IUn of iunop * ('v, 'i) iexp
  | IBin of ('v, 'i) iexp * ibinop * ('v, 'i) iexp
and iunop = private Neg
and ibinop = private Add | Sub

(** {1 Constructors} *)

val fbuiltin : string -> ('v, 'i) exp list -> ('v, 'i) prim_fml
val true_ : ('v, 'i) prim_fml
val false_ : ('v, 'i) prim_fml
val qual : rqualify -> ('v, 'i) exp -> ('v, 'i) prim_fml
val rcomp : ('v, 'i) exp -> comp_op -> ('v, 'i) exp -> ('v, 'i) prim_fml
val icomp : ('v, 'i) iexp -> icomp_op -> ('v, 'i) iexp -> ('v, 'i) prim_fml
val lbinary : ('v, 'i) fml -> lbinop -> ('v, 'i) fml -> ('v, 'i) prim_fml
val qaen :
  ae_quant ->
  ('v, 'i) sim_binding list -> ('v, 'i) block -> ('v, 'i) prim_fml
val qlo :
  lo_quant -> ('v, 'i) binding list -> ('v, 'i) block -> ('v, 'i) prim_fml
val lunary : lunop -> ('v, 'i) fml -> ('v, 'i) prim_fml
val block : ('v, 'i) block -> ('v, 'i) prim_fml
val fite : ('v, 'i) fml -> ('v, 'i) fml -> ('v, 'i) fml -> ('v, 'i) prim_fml
val let_ : ('v, 'i) binding list -> ('v, 'i) block -> ('v, 'i) prim_fml
val all : ae_quant
val some : ae_quant
val no_ : ae_quant
val lone : lo_quant
val one : lo_quant
val and_ : lbinop
val or_ : lbinop
val impl : lbinop
val iff : lbinop
val until : lbinop
val release : lbinop
val since : lbinop
val not_ : lunop
val sometime : lunop
val always : lunop
val once : lunop
val next : lunop
val historically : lunop
val previous : lunop
val num : int -> ('v, 'i) prim_iexp
val none : ('v, 'i) prim_exp
val univ : ('v, 'i) prim_exp
val iden : ('v, 'i) prim_exp
val ident : 'v -> ('i, 'v) prim_exp
val runary : runop -> ('v, 'i) exp -> ('v, 'i) prim_exp
val rbinary : ('v, 'i) exp -> rbinop -> ('v, 'i) exp -> ('v, 'i) prim_exp
val rite : ('v, 'i) fml -> ('v, 'i) exp -> ('v, 'i) exp -> ('v, 'i) prim_exp
val boxjoin : ('v, 'i) exp -> ('v, 'i) exp list -> ('v, 'i) prim_exp
val compr :
  ('v, 'i) sim_binding list -> ('v, 'i) block -> ('v, 'i) prim_exp
val prime : ('v, 'i) exp -> ('v, 'i) prim_exp
val in_ : comp_op
val not_in : comp_op
val req : comp_op
val rneq : comp_op
val ieq : icomp_op
val ineq : icomp_op
val lt : icomp_op
val lte : icomp_op
val gt : icomp_op
val gte : icomp_op
val rone : rqualify
val rsome : rqualify
val rlone : rqualify
val rno : rqualify
val transpose : runop
val tclos : runop
val rtclos : runop
val union : rbinop
val inter : rbinop
val over : rbinop
val lproj : rbinop
val rproj : rbinop
val prod : rbinop
val diff : rbinop
val join : rbinop
val card : ('v, 'i) exp -> ('v, 'i) prim_iexp
val iunary : iunop -> ('v, 'i) iexp -> ('v, 'i) prim_iexp
val ibinary : 'v -> ibinop -> ('i, 'c) iexp -> ('i, 'c) prim_iexp
val neg : iunop
val add : ibinop
val sub : ibinop
val fml : Location.t -> 'v -> 'v located
val exp : Location.t -> 'v -> 'v located
val iexp : Location.t -> 'v -> 'v located
val sat : ('v, 'i) fml list -> ('v, 'i) t

(** {1 Pretty printing} *)

val pp_located :
  (Format.formatter -> 'v -> unit) -> Format.formatter -> 'v located -> unit

val pp :
  'v Fmtc.t ->
  (Format.formatter -> 'i -> unit) -> Format.formatter -> ('v, 'i) t -> unit
val pp_fml :
  'v Fmtc.t -> (Format.formatter -> 'i -> unit) -> ('v, 'i) fml Fmtc.t
val pp_prim_fml :
  'v Fmtc.t ->
  (Format.formatter -> 'i -> unit) ->
  Format.formatter -> ('v, 'i) prim_fml -> unit
val pp_block :
  'v Fmtc.t -> (Format.formatter -> 'i -> unit) -> ('v, 'i) block Fmtc.t
val pp_rqualify : Format.formatter -> rqualify -> unit
val pp_comp_op : Format.formatter -> comp_op -> unit
val pp_icomp_op : Format.formatter -> icomp_op -> unit
val pp_lunop : Format.formatter -> lunop -> unit
val pp_lbinop : Format.formatter -> lbinop -> unit
val pp_lo_quant : Format.formatter -> lo_quant -> unit
val pp_ae_quant : Format.formatter -> ae_quant -> unit
val pp_binding :
  sep:unit Fmtc.t ->
  'v Fmtc.t -> (Format.formatter -> 'i -> unit) -> ('v, 'i) binding Fmtc.t
val pp_sim_binding :
  'v Fmtc.t ->
  (Format.formatter -> 'i -> unit) -> ('v, 'i) sim_binding Fmtc.t
val pp_exp :
  'v Fmtc.t -> (Format.formatter -> 'i -> unit) -> ('v, 'i) exp Fmtc.t
val pp_prim_exp :
  'v Fmtc.t ->
  (Format.formatter -> 'i -> unit) ->
  Format.formatter -> ('v, 'i) prim_exp -> unit
val pp_runop : Format.formatter -> runop -> unit
val pp_rbinop : Format.formatter -> rbinop -> unit
val pp_iexp :
  'v Fmtc.t ->
  (Format.formatter -> 'i -> unit) ->
  Format.formatter -> ('v, 'i) iexp -> unit
val pp_prim_iexp :
  'v Fmtc.t ->
  (Format.formatter -> 'i -> unit) ->
  Format.formatter -> ('v, 'i) prim_iexp -> unit
val pp_iunop : Format.formatter -> iunop -> unit
val pp_ibinop : Format.formatter -> ibinop -> unit
