open Ocamlbuild_plugin;;


(* rule "qtest extract" *)
(*   ~prod:"%_tests.ml" *)
(*   ~deps:["%.ml"] *)
(*   (fun env build -> *)
(*    Cmd(S[A"qtest"; A"extract"; A"-o"; P(env "%_tests.ml"); *)
(*          P(env "%.ml")]));; *)
let import_qtestpack build packfile =
  let tags1 = tags_of_pathname packfile in
  let files = string_list_of_file packfile in
  let include_dirs = Pathname.include_dirs_of (Pathname.dirname packfile) in
  let files_alternatives =
    List.map begin fun module_name ->
      expand_module include_dirs module_name ["ml"; "mli"]
    end files
  in
  let files = List.map Outcome.good (build files_alternatives) in
  let tags2 =
    List.fold_right
      (fun file -> Tags.union (tags_of_pathname file))
      files tags1
  in
  (tags2, files)

let qtest_many target packfile env build =
  let packfile = env packfile and target = env target in
  let tags, files = import_qtestpack build packfile in
  Cmd(S[A "qtest";
        A "-p"; A "let qtest_exn f x = CCOpt.(wrap f x |> is_some)";
        A "extract"; T tags;
        A "-o"; A target; Command.atomize_paths files]);;

rule "ocaml: modular qtest (qtestpack)"
  ~prods:["%.ml"]
  ~deps:["%.qtestpack"]
  ~doc:"Qtest supports building a test module by extracting cases
	directly from several composing several .ml{,i} files together.  \
	To use that feature with ocamlbuild, you should create a .qtestpack \
	file with the same syntax as .mllib or .mlpack files: \
	a whitespace-separated list of the capitalized module names \
	of the .ml{,i} files you want to combine together."
  (qtest_many "%.ml" "%.qtestpack");;

let doc_intro = "doc/intro.text";;


(* let _tag_name = "coverage" *)
(* let _environment_variable = "BISECT_COVERAGE" *)
(* let _enable = "YES" *)

(* let handle_coverage () = *)
(*   if getenv ~default:"" _environment_variable <> _enable then *)
(*     mark_tag_used _tag_name *)
(*   else begin *)
(*     flag ["ocaml"; "compile"; _tag_name] (S [A "-package"; A "bisect_ppx"]); *)
(*     flag ["ocaml"; "link"; _tag_name] (S [A "-package"; A "bisect_ppx"]) *)
(*   end *)


let menhir_flags() =
  (* Define two ocamlbuild flags [only_tokens] and [external_tokens(Foo)]
     which correspond to menhir's [--only-tokens] and [--external-tokens Foo].
     When they are used, these flags should be passed both to [menhir] and to
     [menhir --raw-depend]. *)
  List.iter (fun mode ->
    
    flag [ mode; "only_tokens" ] (S[A "--only-tokens"]);

    pflag [ mode ] "external_tokens" (fun name ->
      S[A "--external-tokens"; A name]
    )
  
  ) [ "menhir"; "menhir_ocamldep" ]


let () =
  dispatch
    (function hook ->
     match hook with
       | After_rules ->
						menhir_flags();
           dep ["ocaml"; "doc"; "extension:html"] & [doc_intro] ;
           flag ["ocaml"; "doc"; "extension:html"] & (S[ A"-intro"; P doc_intro ]);
           flag ["ocaml"; "doc"] & S[A"-hide-warnings"; A"-sort"](* ; *)
           (* handle_coverage () *)
       | _ -> ()
    );;
