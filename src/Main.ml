
(** {b Actual main function.} *)

open Containers




(* inspired by Logs_fmt code *)     
let keyword =
  let open Logs in
  function
    | App -> ""
    | Error -> "ERROR"
    | Warning -> "WARNING"
    | Info -> "INFO"
    | Debug -> "DEBUG"


let short =
  let open Logs in
  function
    | App -> ""
    | Error -> "E"
    | Warning -> "W"
    | Info -> "I"
    | Debug -> "D"



let pp_header ppf (l, h) =
  let open Logs in 
  let open Logs_fmt in
  let pp_h ppf style h = Fmtc.pf ppf "[%a] " Fmtc.(styled style string) h in
  match l with
    | App ->
        begin match h with
          | None -> ()
          | Some h -> Fmtc.pf ppf "[%a] " Fmtc.(styled app_style string) h
        end
    | Error
    | Warning
    | Info
    | Debug ->
        pp_h ppf (Msg.style l)
        @@ CCOpt.map_or ~default:(keyword l) (fun s -> short l ^ s) h


let main style_renderer verbosity infile =
  Printexc.record_backtrace true;

  Fmt_tty.setup_std_outputs ?style_renderer ();

  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  Logs.set_level ~all:true verbosity;

  Logs.app
    (fun m ->
       m "%a@\nProcessing file: %s"
         Fmtc.(styled `Bold string) "electrod (C) 2016-2017 ONERA"
         infile);

  (try
     let inch = Unix.open_process_in "tput cols" in
     let cols =
       inch
       |> IO.read_line
       |> Fun.tap (fun _ -> ignore @@ Unix.close_process_in inch)
       |> Option.get_or ~default:"80"
       |> int_of_string in
     (* Msg.debug (fun m -> m "Columns: %d" cols); *)
     Format.(pp_set_margin stdout) cols;
     Format.(pp_set_margin stderr) cols
   with _ ->
     Msg.debug
       (fun m -> m "Columns not found, leaving terminal as is..."));

  (* begin work *)
  try
    let raw_to_elo_t = Transfo.tlist [ Raw_to_elo.transfo ] in
    let elo_to_elo_t = Transfo.tlist [ Simplify1.transfo; Simplify2.transfo ] in
    let elo_to_smv_t = Transfo.tlist
                         [ Elo_to_SMV1.transfo; (* Elo_to_SMV2.transfo *)] in

    let model =
      Parser_main.parse_file infile
      |> Fun.tap (fun _ -> Msg.info (fun m -> m "Parsing OK."))
      |> Transfo.(get_exn raw_to_elo_t "raw_to_elo" |> run)
      |> Fun.tap (fun _ -> Msg.info (fun m -> m "Static analysis OK."))
      (* |> Fun.tap (fun elo -> Msg.debug (fun m -> m "After raw_to_elo =@\n%a@." (Elo.pp) elo)) *)
      |> Transfo.(get_exn elo_to_elo_t "simplify1" |> run)
      (* |> Fun.tap (fun elo -> Msg.debug (fun m -> m "After simplify1 =@\n%a@." (Elo.pp) elo)) *)
      |> Fun.tap (fun _ -> Msg.info (fun m -> m "Simplification OK."))
      |> Transfo.(get_exn elo_to_smv_t "to_smv1" |> run) 
      |> Fun.tap (fun _ -> Msg.info (fun m -> m "Conversion OK."))
    in
    (* let sup_r = Domain.sup (Name.name "r") elo.domain in *)
    (* let tc_r = TupleSet.transitive_closure sup_r in *)
    (* Msg.debug (fun m -> *)
    (*     m "Borne sup de la tc de r : %a " TupleSet.pp tc_r); *)

    let res = Elo_to_SMV1.analyze None infile model in
    (match res with
      | Error (errcode, errmsg) ->
          Logs.app (fun m -> m "ERROR (code %d):@\n%s" errcode errmsg)
      | Ok trace ->
          Logs.app (fun m -> m "SUCCESS:@\n%a" Trace.pp trace));

    Logs.app (fun m -> m "Elapsed (wall-clock) time: %a"
                         Mtime.Span.pp (Mtime_clock.elapsed ()));

    Msg.debug (fun m -> m "@.%a"
                          (Elo_to_SMV1.pp ~margin:78) model)

  with
    | Exit ->
        Logs.app
          (fun m -> m "Aborting (%a)." Mtime.Span.pp (Mtime_clock.elapsed ()));
        exit 1
    | e ->
        raise e
      


