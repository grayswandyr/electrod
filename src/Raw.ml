
open Containers

              
type ('v, 'n) raw_problem = {
  file : string option;
  raw_univ : raw_urelements list;
  raw_decls : raw_declaration list;
  raw_goals : ('v, 'n) Raw_goal.t list;    (** nonempty *)
}

and raw_urelements = 
  | UIntvl of raw_interval
  | UPlain of Raw_ident.t

and raw_declaration = 
  | DConst of Raw_ident.t * raw_scope
  | DVar of Raw_ident.t * raw_scope * raw_scope option

and raw_scope = 
  | SExact of raw_bound
  | SInexact of raw_bound * raw_bound

and raw_bound = 
  | BUniv
  | BRef of Raw_ident.t         (** reference to a previously-defined {i set} *)
  | BProd of raw_bound * raw_bound
  | BUnion of raw_bound * raw_bound
  | BElts of raw_element list

and raw_element = 
  | EIntvl of raw_interval     (** 1-tuples *)
  | ETuple of Raw_ident.t list (** A n-tuple (incl. n = 1). inv: nonempty list *)

and raw_interval = Raw_ident.t * Raw_ident.t




let interval id1 id2 = (id1, id2)

let etuple ats =
  assert (ats <> []);
  ETuple ats

let eintvl intvl = EIntvl intvl

let buniv = BUniv

let bref name = BRef name

let bprod b1 b2 = BProd (b1, b2)

let bunion b1 b2 = BUnion (b1, b2)

let belts elts = BElts elts

let sexact b = SExact b

let sinexact b1 b2 = SInexact (b1, b2)

let dconst atom scope = DConst (atom, scope)

let dvar atom scope fby = DVar (atom, scope, fby)

let uintvl intvl = UIntvl intvl

let uplain atom = UPlain atom

let problem file raw_univ raw_decls raw_goals =
  { file; raw_univ; raw_decls; raw_goals }


let decl_id = function
  | DConst (id, _)
  | DVar (id, _, _) -> id

let raw_bound_location = Location.span 
