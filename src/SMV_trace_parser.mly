
%parameter <D : sig
  val base : (Name.t, TupleSet.t) CCList.Assoc.t 
end>

%{
  open SMV_trace_tokens
  open Containers

  module LA = List.Assoc
  
  (* converts a list containing pairs (name, tuple) in a list of pairs (name,
     set of tuples), i.e. gathers (in [acc]) all tuples corresponding to a given
     name in a same set. The list is nonempty. *)
  let upd tuple = function
    | None -> Some (TupleSet.singleton tuple)
    | Some ts -> Some (TupleSet.add tuple ts)
    
  let convert_name_tuple_l ntl =
    let rec walk acc = function
      | [] -> acc
      (* if None: means that atom was FALSE, else TRUE *)
      | None::tl -> walk acc tl
      | Some (name, tuple)::tl ->
         begin
           (* Msg.debug (fun m -> m "conv (%a, %a)" Name.pp name Tuple.pp tuple); *)
           let acc2 = LA.update ~eq:Name.equal ~f:(upd tuple) name acc in
           walk acc2 tl
         end
    in walk D.base ntl


  let met_one_loop = ref false

  let rec tag_last_as_loop = function
    | [] -> assert false
    | [ st ] -> [ Trace.to_loop st ]
    | hd::tl -> hd :: tag_last_as_loop tl
       

%}
  
%start <Trace.t> trace



%%

%public trace:
states = state+ EOF 
    {
      Trace.make
      (if !met_one_loop
       then states
       else tag_last_as_loop states)
    }

 state:
    loop = iboption(LOOP) STATE ntl = atomic*
    {
      let valu = Trace.valuation @@ convert_name_tuple_l ntl in
      if loop then
        (met_one_loop := true;
         Trace.loop_state valu)
      else
        Trace.plain_state valu
    }

atomic:
v = ATOMIC EQUAL FALSE
    { None }                
| v = ATOMIC EQUAL TRUE
    { Some v }
    
////////////////////////////////////////////////////////////////////////
// MENHIR MACROS
////////////////////////////////////////////////////////////////////////
  
%public %inline iboption(X):
(* empty *)
{ false }
| X
    { true }


    
