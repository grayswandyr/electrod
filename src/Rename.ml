
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

let rename_domain dom =
  let bindings = Domain.to_list dom in
  let original_names = ref [] in
  let new_bindings =
    bindings
    |> List.mapi (fun i (name, rel) ->
          let new_name = Name.name @@ encode_relation i in
          original_names := (new_name, name) :: !original_names;
          (new_name, rel))
  in
  (!original_names, Domain.of_list new_bindings)
