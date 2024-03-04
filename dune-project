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
  (cmdliner ( <= 1.1.1))
  (containers ( <= 3.10))
  (containers-data ( <= 3.10))
  (dune (<= 3.5.0))
  (dune-build-info ( <= 3.5.0))
  (fmt ( <= 0.9.0))
  (gen ( <= 1.0))
  (hashcons ( <= 1.3))
  (iter ( <= 1.6))
  (logs ( <= 0.7.0))
  (mdx :with-test)
  (menhir ( <= 20220210))
  (mtime ( <= 1.4.0))
  (ocaml (= 4.14.1))
  (ocamlformat (= 0.24.1))
  (ocaml-lsp-server ( <= 1.14.2))
  (ppx_deriving ( <= 5.2.1))
  (printbox ( <= 0.6.1))
  (printbox-text ( <= 0.6.1))
  (stdcompat ( <= 19))
  (stdlib-shims ( <= 0.3.0))
  (utop ( <= 2.10.0))
  (visitors ( <= 20210608))
 )
 (synopsis "Formal analysis for the Electrod formal pivot language")
 (description
  "Electrod is a model finder inspired by Kodkod. It takes as input a\nmodel expressed in a mixture of relational first-order logic (RFOL)\nover bounded domains and linear temporal logic (LTL) over an unbounded\ntime horizon. Then it compiles the model to a problem for a solver (currently\nthe NuSMV and nuXmv tools) to produce example or counter-example traces.\nElectrod is primarily meant to be used as a backend for the Electrum formal method and tool."))
