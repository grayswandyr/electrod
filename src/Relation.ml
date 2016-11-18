
open Containers


type t =
  | Const of { name : Name.t; scope : Scope.t }
  | Var of { name : Name.t; scope : Scope.t; fby : Scope.t option }


let const name scope = Const { name; scope }

let var name scope fby =  Var { name; scope; fby }


let arity = function
  | Const { scope; _ }
  | Var { scope; _ } -> Scope.arity scope


let is_set rel =
  CCOpt.compare CCInt.compare (arity rel) (Some 1) = 0 
  
let is_nary rel =
  CCOpt.compare CCInt.compare (arity rel) (Some 1) > 0 

let is_const = function
  | Const _ -> true
  | Var _ -> false

let is_var = function
  | Const _ -> false
  | Var _ -> true


let pp ?(print_name = true) out rel = 
  let open Fmtc in
  let pp_name =
    if print_name then (sp **> colon **> nbsp **> Name.pp) else nop
  in
  match rel with
    | Const { name; scope } ->
        pp_name out name;
        (sp **> string) out "const";
        Scope.pp out scope
    | Var { name; scope; fby } ->
        pp_name out name;
        (sp **> string) out "var";
        Scope.pp out scope;
        option (sp **< const string "then" **< sp **< Scope.pp) out fby


let to_string  ?(print_name = true) rel =
  Fmtc.to_to_string (pp ~print_name) rel
