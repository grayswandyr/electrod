
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


  (* From what we gathered, we should remove the last state from the returned
     trace (it is the loop-target state), and if the first state is the target,
     it is not said so we have to tag it as a loop-target by ourselves. *)
  let met_one_loop = ref false

  let rec remove_last = function
    | [] -> assert false
    | [_] -> []
    | hd::tl -> hd :: remove_last tl
    
  let rec first_state_as_loop = function
    | [] -> assert false
    | hd::tl -> (Outcome.to_loop hd) :: tl
       
%}
  
%start <Outcome.t> trace



%%

%public trace:
states = state+ EOF 
    {
      Outcome.trace
      (if !met_one_loop
       then remove_last states
       else first_state_as_loop @@ remove_last states)
    }

    state:
    loop = iboption(LOOP) STATE ntl = atomic*
    {
      let valu = Outcome.valuation @@ convert_name_tuple_l ntl in
      if loop then
        (met_one_loop := true;
         Outcome.loop_state valu)
      else
        Outcome.plain_state valu
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


    
