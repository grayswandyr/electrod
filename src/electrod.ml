 
(** {b Entrypoint for the 'electrod' program.} Performs CLI management.*)

open Containers
open Cmdliner
 
let infile =
  let doc = "File to process." in
  Arg.(required & pos 0 (some non_dir_file) None
       & info [] ~docv:"FILE" ~doc )


let tool =
  let doc = {|Analysis tool to rely upon. TOOL must be one of `nuXmv' or \
              `NuSMV'.|}
  in
  Arg.(value & opt (enum [ ("nuXmv", Main.NuXmv); ("NuSMV", Main.NuSMV)]) Main.NuXmv
       & info ["t"; "tool"] ~docv:"TOOL" ~doc)

let script =
  let doc = {|Script to pass to the analysis tool (default: homemade nuXmv \
              script calling 'check_ltlspec_klive'). A script file for nuXmv \
              or NuSMV MUST set both options `default_trace_plugin' and \
              `on_failure_script_quits' to '1'. NOTICE: no verification \
              of the script file is made whatsoever. |} in
  Arg.(value & opt (some non_dir_file) None
       & info ["s"; "script"] ~docv:"FILE" ~doc)

let keep_files =
  let doc = {|If present, do not delete the model and script files after use.|}
  in
  Arg.(value & flag & info ["keep"] ~doc)

(* verbosity options (already def'd in Logs_cli, thx!) *)
let verb_term = 
  Logs_cli.level ()
    
(* same for colors *)
let color_term =
  Fmt_cli.style_renderer ()


let main_term = 
  Term.(const Main.main
        $ color_term
        $ verb_term
        $ tool
        $ infile
        $ script
        $ keep_files)

let third_party_blurb =
  {|Electrod relies on the following third-party free software, \
    released under their respective licence (see the respective \
    OPAM repositories for the full text of the licences): cmdliner, \
    mtime, fmt, logs, containers, sequence, gen, hashcons, \
    ppx_deriving, visitors.|}

let author_blurb = {|Julien BRUNEL (ONERA), David CHEMOUIL (ONERA).|}
  
let main_info =
  let doc = "formal analysis of Electrod models." in
  let man = [
    `S "COPYRIGHT";
    `P "Electrod (C) 2016-2017 ONERA.";
    `P "Electrod is free software: you can redistribute \
        it and/or modify it under the terms of the Mozilla Public \
        License, v. 2.0. If a copy of the MPL was not distributed with this \
        file, You can obtain one at http://mozilla.org/MPL/2.0/.";
    `P "Electrod is distributed in the hope that it \
        will be useful, but WITHOUT ANY WARRANTY; without even the \
        implied warranty of MERCHANTABILITY or FITNESS FOR A \
        PARTICULAR PURPOSE. ";
    `S "THIRD-PARTY SOFTWARE";
    `P third_party_blurb;
    `S "AUTHORS";
    `P author_blurb
  ]
  in
  Term.info "electrod" ~doc ~man



let () =
  (* process commandline arguments and run the actual main (Main.main) function *)
  match Term.eval ~catch:true (main_term, main_info) with
    | `Error _ -> exit 1
    | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
