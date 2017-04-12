
open Containers


type t =
  | Const of { name : Name.t; arity : int; scope : Scope.t }
  | Var of { name : Name.t; arity : int; scope : Scope.t; fby : Scope.t option }


let const name arity scope = Const { name; arity; scope }

let var name arity scope fby =  Var { name; arity; scope; fby }


let arity = function
  | Const { arity; _ }
  | Var { arity; _ } -> arity


let is_set rel =
  arity rel = 1
  
let is_nary rel =
  arity rel > 1

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
  let pp_def const_or_var name scope fby arity = 
    pp_name out name;
    (styled `Bold @@ string) out const_or_var;
    Format.pp_open_hbox out ();
    string out "<";
    int out arity;
    (sp **> string) out ">";
    Format.pp_open_box out 2;
    (box2 @@ Scope.pp) out scope;
    option ((styled `Bold @@ sp **< const string "then")
            **< sp **< (box2 @@ Scope.pp)) out fby;
    Format.pp_close_box out ();
    Format.pp_close_box out ()
  in
  match rel with
    | Const { name; scope; arity } ->
        pp_def "const" name scope None arity
    | Var { name; scope; fby; arity  } ->
        pp_def "var" name scope fby arity


let to_string ?(print_name = true) rel =
  Fmtc.to_to_string (pp ~print_name) rel
