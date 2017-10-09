open Containers

type t = states option

and states = state list         (** nonempty *)

(** A state is either a plain state, or the target of a lasso from the last
    state of the trace. *)
and state =
  | Plain of valuation
  | Loop of valuation

(** A valuation maps set/relation names to the tuples they contain. *)
and valuation = (Name.t, TupleSet.t) List.Assoc.t


let valuation valu =
  valu

let plain_state v = Plain v

let loop_state v = Loop v

let to_loop = function Loop v | Plain v -> Loop v


let no_trace = None

let trace states =
  assert (states <> []
          && List.exists (function Loop _ -> true | Plain _ -> false) states);
  Some states


open Fmtc

module PPPlain = struct    
  let pp_valuation out valu =
    pf out "%a"
      (hvbox @@ list ~sep:sp
       @@ pair ~sep:equal Name.pp TupleSet.pp)
    @@ List.sort (fun (n1, _) (n2, _) -> Name.compare n1 n2) valu

  let pp_state out = function
    | Plain v -> (const string "  " **< brackets_ pp_valuation) out v 
    | Loop v -> (const string "->" **< brackets_ pp_valuation) out v

  let pp out = function
    | None -> pf out "--no trace--"
    | Some trace ->
        (vbox @@ list ~sep:sp pp_state) out trace
end

module PPXML = struct

  let kwd = string

  let attr = styled `Green string

  let pp_atom out at =
    let tag = "a" in
    pf out "<%a>%a</%a>"
      kwd tag
      (styled `Cyan Atom.pp) at
      kwd tag

  let pp_tuple out tuple =
    let tag = "t" in
    pf out "@[<h><%a>%a</%a>@]"
      kwd tag
      (list ~sep:cut pp_atom) (Tuple.to_list tuple)
      kwd tag

  let pp_one_valuation out (name, ts) =
    let tag = "rel" in
    let attribute = "name" in
    if TupleSet.is_empty ts then
      pf out "@[<h><%a %a=\"%a\"/>@]"
        kwd tag
        attr attribute
        Name.pp name
    else
      pf out "@[<v><%a %a=\"%a\">@,  @[<v>%a@]@,</%a>@]"
        kwd tag
        attr attribute
        Name.pp name
        (Tuple.Set.pp ~sep:"" pp_tuple) (TupleSet.tuples ts)
        kwd tag

  let pp_valuation out valu =
    list ~sep:cut pp_one_valuation out
    @@ List.sort (fun (n1, _) (n2, _) -> Name.compare n1 n2) valu


  let pp_state out st =
    let tag = "st" in
    let attribute = "loop-target" in
    let valu, loop = match st with
      | Loop v -> (v, true)
      | Plain v -> (v, false)
    in
    pf out "@[<v><%a %a=\"%a\">@,  @[<v>%a@]@,</%a>@]"
      kwd tag
      attr attribute
      (styled `Cyan bool) loop
      pp_valuation valu
      kwd tag

  let pp out t =
    pf out "<?%a %a=\"1.0\" %a=\"UTF-8\"?>@\n"
      kwd "xml"
      attr "version"
      attr "encoding";
    (match t with
      | None -> pf out "@[<h><%a/>@]" kwd "notrace"
      | Some trace ->
          let tag = "trace" in
          pf out "@[<v><%a>@,  @[<v>%a@]@,</%a>@]"
            kwd tag
            (list ~sep:sp pp_state) trace
            kwd tag);
    Format.pp_print_flush out ()

end

let pp ~(format : [`XML | `Plain]) out trace = match format with
  | `Plain -> PPPlain.pp out trace
  | `XML -> PPXML.pp out trace
