
(** {b Actual main function.} *)

open Containers


open ToLTL


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

  (try
     let cols = int_of_string @@ Sys.getenv "COLUMNS" in
     Format.(pp_set_margin stdout) cols;
     Format.(pp_set_margin stderr) cols
   with Not_found ->
     Msg.debug
       (fun m -> m "Unfound environment variable: COLUMNS"));

  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  Logs.set_level ~all:true verbosity;

  Logs.app
    (fun m -> m "%a" Fmtc.(styled `Bold string) "electrod (C) 2016 ONERA");
  (* begin work *)
  try
    let raw_to_elo_t = Transfo.tlist [ Raw_to_elo.transfo ] in
    let elo_to_elo_t = Transfo.tlist [ Simplify.transfo ] in

    let elo =
      Parser_main.parse_file infile
      |> Transfo.(get_exn raw_to_elo_t "raw_to_elo" |> run)
      |> Transfo.(get_exn elo_to_elo_t "simplify" |> run)
    in

    let test_f =
      let open Elo in
      let module Elo_to_SMV1_formulas = ToLTL.MakeLtlConverter(ToSMV1.Logic) in
      List.map (Elo_to_SMV1_formulas.convert elo) elo.goals
    in


    Msg.debug
      (fun m -> m "Elo AST =@;%a" (Elo.pp) elo);

    List.iter (fun f -> Msg.debug (fun m -> m "SMV formula: %a" ToSMV1.Logic.pp f)) test_f;

    Logs.app (fun m -> m "Elapsed (wall-clock) time: %a"
                         Mtime.pp_span (Mtime.elapsed ()))
  with
    | Exit ->
        flush_all ();
        Logs.app
          (fun m -> m "Aborting (%a)." Mtime.pp_span (Mtime.elapsed ()));
        exit 2
    | e ->
        flush_all ();
        raise e
      


