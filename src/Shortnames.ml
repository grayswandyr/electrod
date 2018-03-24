
open Containers

(* Converts a natural number into a list of base-26 digits, in reversed order.
   E.g. [convert 27 = [0;1]] *)
let convert n =
  assert (n >= 0);
  let rec aux n =
    let q = n / 26 and r = n mod 26 in
    if q = 0 then
      [r]
    else 
      r :: aux q
  in aux n

(* Creates an encoding function of numbers into base 26, starting at character
   [base]. *)
let make_encode base = 
  let base_code = CCChar.code base in
  fun n ->
    let list = convert n in
    List.rev_map (fun i -> CCChar.of_int_exn (i + base_code)) list
    |> CCString.of_list

(* lowercase letters for atoms *)
let encode_atom = make_encode 'a'

(* uppercase letters for relations *)
let encode_relation = make_encode 'A'

let compute_relation_renaming elo =
  Domain.to_list elo.Elo.domain 
  |> List.mapi (fun i (name, _) ->
        if Name.equal name Name.iden || Name.equal name Name.univ then
          (name, name)          (* do not rename univ and iden *)
        else
          let new_name = Name.name @@ encode_relation i in
          (name, new_name))

let compute_atom_renaming elo = 
  Domain.univ_atoms elo.Elo.domain
  |> TupleSet.to_list
  |> List.mapi (fun i tuple ->
        let atom = Tuple.ith 0 tuple in
        let new_atom = Atom.atom @@ encode_atom i in
        (atom, new_atom))


let rename_elo elo =
  let atom_renaming = compute_atom_renaming elo in 
  let name_renaming = compute_relation_renaming elo in
  Fmt.pf Fmt.stdout "%a@\n"
    Fmtc.(brackets @@ list ~sep:semi @@ parens @@ pair ~sep:comma Atom.pp Atom.pp) atom_renaming;
  Fmt.pf Fmt.stdout "%a@\n"
    Fmtc.(brackets @@ list ~sep:semi @@ parens @@ pair ~sep:comma Name.pp Name.pp) name_renaming;
  Elo.{
    elo with
      domain = Domain.rename atom_renaming name_renaming elo.domain;
      goal = Elo.rename#visit_t name_renaming elo.goal;
      invariants = List.map (Elo.rename#visit_fml name_renaming) elo.invariants;
      sym = List.map (Symmetry.rename atom_renaming name_renaming) elo.sym;
      instance = Instance.rename atom_renaming name_renaming elo.instance;
      atom_renaming;
      name_renaming
  }
