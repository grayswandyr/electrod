(** Type of transformations. *)

open Containers

type ('src, 'dst) t = {
  name : string;
  run : 'src -> 'dst
}

let make name run =
  assert (name <> "");
  { name; run }

let name { name; _ } = name

let run t x =
  t.run x

let fby t1 t2 = {
  name = t1.name ^ "$$" ^ t2.name;
  run = fun x -> t1.run x |> t2.run
}

let identity = {
  name = "$$id";
  run = fun x -> x
}

(* association list *)
type ('src, 'dst) tlist =  (string * ('src, 'dst) t) list

let tlist ts =
  assert (ts <> []);
  let open List in
  let add transfos t =
    assert (not @@ Assoc.mem ~eq:String.equal transfos t.name);
    Assoc.set ~eq:String.equal transfos t.name t
  in
  fold_left add [] ts

let get_exn ts t = List.Assoc.get_exn ~eq:String.equal ts t
