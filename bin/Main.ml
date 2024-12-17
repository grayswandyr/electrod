(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2024 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** {b Actual main function.} *)

open Containers
open Libelectrod

(* inspired by Logs_fmt code *)
let keyword =
  let open! Logs in
  function
  | App -> ""
  | Error -> "ERROR"
  | Warning -> "WARNING"
  | Info -> "INFO"
  | Debug -> "DEBUG"

let short =
  let open! Logs in
  function
  | App -> "" | Error -> "E" | Warning -> "W" | Info -> "I" | Debug -> "D"

let pp_header ppf (l, h) =
  let open! Logs in
  let open Logs_fmt in
  let pp_h ppf style h = Fmtc.pf ppf "[%a] " Fmtc.(styled style string) h in
  match l with
  | App -> (
      match h with
      | None -> ()
      | Some h -> Fmtc.pf ppf "[%a] " Fmtc.(styled app_style string) h)
  | Error | Warning | Info | Debug ->
      pp_h ppf (Msg.style l)
      @@ Option.map_or ~default:(keyword l) (fun s -> short l ^ s) h

type tool = NuXmv | NuSMV

(* Taken from nunchaku-inria/logitest/src/Misc.ml (BSD licence).
   Make sure that we are a session leader; that is, our children die if we die.Raises if the process is already a session leader, so we ignore the exception. *)
let ensure_session_leader () =
  try
    CCUnix.ensure_session_leader ();
    Msg.debug (fun m -> m "`setsid` succeeded.")
  with Unix.Unix_error (EPERM, "setsid", _) ->
    Msg.debug (fun m -> m "`setsid` failed, continuing.")

let main style_renderer verbosity tool file scriptfile keep_files no_analysis
    print_generated outcome_format long_names bmc temporal_symmetry
    symmetry_offset single_formula =
  let long_names =
    (* Debug ==> long names *)
    match verbosity with Some Logs.Debug -> true | None | Some _ -> long_names
  in
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  Logs.set_level ~all:true verbosity;
  let version =
    match Build_info.V1.version () with
    | None -> "(development version)"
    | Some v -> Build_info.V1.Version.to_string v
  in
  Logs.app (fun m ->
      m "%a"
        Fmtc.(styled `Bold string)
        ("electrod (C) 2016-2024 ONERA " ^ version));
  ensure_session_leader ();
  Msg.debug (fun m -> m "CWD = %s" (Sys.getcwd ()));
  Msg.debug (fun m -> m "PATH = %s" (Sys.getenv "PATH"));
  Logs.app (fun m -> m "Processing file: %s" file);
  (* begin work *)
  try
    let raw_to_ast_t = Transfo.tlist [ Raw_to_ast.transfo ] in
    let ast_to_ast_t = Transfo.tlist [ Simplify1.transfo; Simplify2.transfo ] in
    let elo_to_smv_t = Transfo.tlist [ Elo_to_smv1.transfo ] in
    let ast =
      Parser_main.parse_file file
      |> Fun.tap (fun _ -> Msg.info (fun m -> m "Parsing done"))
      |> Transfo.(get_exn raw_to_ast_t "raw_to_elo" |> run)
      |> Fun.tap (fun _ -> Msg.info (fun m -> m "Static analysis done"))
      |> Fun.tap (fun elo ->
             Msg.debug (fun m -> m "After raw_to_elo =@\n%a@." Ast.pp elo))
      |> Shortnames.rename_elo long_names
      |> Transfo.(get_exn ast_to_ast_t "simplify1" |> run)
      |> Fun.tap (fun elo ->
             Msg.debug (fun m -> m "After simplify1 =@\n%a@." Ast.pp elo))
      |> Fun.tap (fun _ -> Msg.info (fun m -> m "Simplification done"))
    in
    let expect = Gen_goal.get_expected ast.goal in
    let elo = Ast_to_elo.convert ast in
    let before_conversion = Mtime_clock.now () in
    let model =
      Transfo.(get_exn elo_to_smv_t "to_smv1" |> run)
        (elo, temporal_symmetry, symmetry_offset, single_formula)
    in
    let conversion_time = Mtime.span before_conversion @@ Mtime_clock.now () in
    (match verbosity with
    | Some _ ->
        Msg.info (fun m ->
            m "Conversion done in %a" Mtime.Span.pp conversion_time)
    | None -> Logs.app (fun m -> m "Conversion done"));
    let cmd, script =
      match (tool, scriptfile, bmc) with
      | NuXmv, None, None -> ("nuXmv", Solver.Default Smv.nuXmv_default_script)
      | NuSMV, None, None -> ("NuSMV", Solver.Default Smv.nuSMV_default_script)
      | NuXmv, None, Some _ ->
          ("nuXmv", Solver.Default Smv.nuXmv_default_bmc_script)
      | NuSMV, None, Some _ ->
          ("NuSMV", Solver.Default Smv.nuSMV_default_bmc_script)
      | NuXmv, Some s, _ -> ("nuXmv", Solver.File s)
      | NuSMV, Some s, _ -> ("NuSMV", Solver.File s)
    in
    if print_generated then
      Logs.app (fun m ->
          m
            "Generated \
             file:@.--------------------------------------------------------------------------------@\n\
             %a--------------------------------------------------------------------------------"
            (Elo_to_smv1.pp ~margin:80)
            model);
    let res =
      Elo_to_smv1.analyze ~conversion_time ~cmd ~keep_files ~no_analysis ~elo
        ~script ~file ~bmc ~pp_generated:long_names model
    in
    if not no_analysis then (
      (* store the trace *)
      IO.with_out
        (Filename.chop_extension file ^ ".xml")
        (fun chan -> Format.with_out_chan chan (Outcome.pp ~format:`XML) res);
      Logs.app (fun m ->
          if Outcome.some_trace res then
            match expect with
            | Some true -> m "Specification is: SAT as expected"
            | Some false -> m "Specification is: SAT contrary to expectation"
            | None -> m "Specification is: SAT"
          else
            match expect with
            | Some true -> m "Specification is: UNSAT contrary to expectation"
            | Some false -> m "Specification is: UNSAT as expected"
            | None -> m "Specification is: UNSAT");
      Msg.info (fun m ->
          m "Outcome:@.%a" (Outcome.pp ~format:outcome_format) res));
    (* Msg.debug (fun m ->
       m "Count references to hashconsed formulas:@\n@[<v2>%a@]"
       Elo.pp_fml_stats 10
       ); *)
    let memory = Gc.allocated_bytes () in
    Msg.info (fun m ->
        m "Total allocated memory: %.3fGB" (memory /. 1_000_000_000.));
    Logs.info (fun m ->
        m "Elapsed (wall-clock) time: %a" Mtime.Span.pp (Mtime_clock.elapsed ()))
  with
  | Exit ->
      Logs.app (fun m ->
          m "Aborting (%a)." Mtime.Span.pp (Mtime_clock.elapsed ()));
      exit 1
  | e -> raise e
