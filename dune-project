(lang dune 3.5)

(name electrod)

(using menhir 2.1)

(generate_opam_files true)

(license MPL-2.0)

(maintainers "David Chemouil <david.chemouil+git@onera.fr>")

(authors "David Chemouil" "Julien Brunel")

(source
 (github grayswandyr/electrod))

(documentation "https://grayswandyr.github.io/electrod/api")

(package
 (name electrod)
 (depends
  (ocaml
   (= 4.14.1))
  (dune (<= 3.5.0))
  dune-build-info
  cmdliner
  containers
  containers-data
  fmt
  gen
  hashcons
  logs
  (mdx :with-test)
  menhir
  mtime
  ppx_deriving
  printbox
  printbox-text
  iter
  stdcompat
  stdlib-shims
  visitors
  ;; Dev dependencies
  (ocamlformat
   (= 0.26.1))
  utop
  merlin
  ocaml-lsp-server)
 (synopsis "Formal analysis for the Electrod formal pivot language")
 (description
  "Electrod is a model finder inspired by Kodkod. It takes as input a\nmodel expressed in a mixture of relational first-order logic (RFOL)\nover bounded domains and linear temporal logic (LTL) over an unbounded\ntime horizon. Then it compiles the model to a problem for a solver (currently\nthe NuSMV and nuXmv tools) to produce example or counter-example traces.\nElectrod is primarily meant to be used as a backend for the Electrum formal method and tool."))
