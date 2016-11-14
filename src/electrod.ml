 
(** {b Entrypoint for the 'electrod' program.} Performs CLI management.*)

open Containers
open Cmdliner
 
let infile =
  let doc = "File to process." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

(* verbosity options (already def'd in Logs_cli, thx!) *)
let verb_term = 
  let env = Arg.env_var "ELECTROD_VERBOSITY" in
  Logs_cli.level ~env ()
    
(* same for colors *)
let color_term =
  let env = Arg.env_var "ELECTROD_COLOR" in
  Fmt_cli.style_renderer ~env ()


let main_term = 
  Term.(const Main.main $ color_term $ verb_term $ infile)

let () =
  (* process commandline arguments and run the actual main (Main.main) function *)
  match Term.eval ~catch:true (main_term, Term.info "electrod") with
    | `Error _ -> exit 1
    | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
