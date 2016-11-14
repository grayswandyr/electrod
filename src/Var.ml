
(** Provides fresh identifiers for variables (in formulas) at every stage. *)

(** type of an identifier *)
type t = {
  id : int;                     (** [id] identifies an identifier uniquely *)
  name : string;                (** [name] is a base string used to give a 
                                    human-friendly display *)
  sep : string
}

let fresh =
  let c = ref 0 in
  fun ?(sep = "$") s ->
    assert (!c < max_int);
    let res = { id = !c; name = s; sep } in
    incr c;
    res

let compare id1 id2 =
  CCInt.compare id1.id id2.id


let pp out { id; name; sep } =
  Fmtc.(pf out "%s%s%d" name sep id)

module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
