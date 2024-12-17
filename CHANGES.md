### 1.2.3 (2024-12-17)
- fix tagging issue

### 1.2.2 (2024-12-17)
- fix tagging issue

### 1.2.1 (2024-12-17)
- improve cram tests infrastructure (and revert unwanted improvements)

### 1.2.0 (2024-10-03)
- fix bug: several lassos in some SMV traces. The BMC algorithm now is now "sbmc_inc" and the complete algorithm is (for nuXmv) "ic3 -i".

### 1.1.1 (2024-09-27)
- fix bug: some arithmetic auxiliaries were absent from the generated SMV file

### 1.1.0 (2024-05-24)
- handle int atoms, Int signature and all Alloy arithmetic operations
- fix cardinality translation bug
- stratify LTL formulas for SMV

### 1.0.0 (2021-01-11)
- build (using Github Actions) on Windows
- fix nuXmv/NuSMV output parsing on Windows, which prevented correct behavior

### 0.9.0 (2020-12-04)
- output much shorter files (unless pretty-printing is on)

### 0.8.0 (2020-12-03)
- huge translation boost and lower memory footprint
- beta: use of Github Actions should enable to produce Windows versions
- fix one compiler version (currently 4.11.0) 

### 0.7.2 (2020-11-26)
- adapt to (and require) containers 3.0
- remove vacuity tests in Raw_to_ast (no reason to fail there)

### 0.7.1 (2020-07-24)
- fix precedences (after update to Electrum)

### 0.7.0 (2020-07-23)
- use same logical connectives and precedence as in Electrum

### 0.6.2 (2020-06-11)
- inhibit warning 63 in release mode, see <https://github.com/ocaml/dune/issues/2450>

### 0.6.1 (2020-06-11)
- F. Pottier's fix to work with Menhir 20200525

### 0.6 (2020-04-02)
- add option --symmetry-offset/--so to offset the single-state symmetry breaking predicate.

### 0.5 (2020-02-28)
- add option --temporal-symmetry/--ts to generate a full temporal symmetry formula
- adapt to changes in the API of the Containers package

### 0.4.1 (2019-12-06)
- fighting once again with dune-release...

### 0.4 (2019-12-06)
- fix bug in symmetry handling resulting in a too strong symmetry breaking predicate, users should re-assess their models!
- add optional 'expect sat' or 'expect unsat' sentence after a goal

### 0.3.2 (2019-10-16)
- fix issue when creating 0.3.1 which led to opam file being still absent

### 0.3.1 (2019-10-16)
- fix missing opam file

### 0.3.0 (2019-10-16)
- use nuXmv's check_ltlspec_ic3 algorithm (released in nuXmv 2.0 on 2019/10/14). nuXmv before 2.0 is not supported at all, please update to 2.0.

### 0.2.3 (2019-09-22)
- fix compilation problems (depend on ocaml >= 4.05.0)
- use dune-build-info

### 0.2.2 (2019-09-21)
- switch to Iter and Stdlib
- reformat code
- fix bug related to update of Fmt

### 0.2.1 (2018-11-01)
- fix bug between Dune, Menhir and 4.07.1
- make compilable until 4.04
- add compilation testing for various versions of the compiler

### 0.2.0 (2018-10-25)
- migration to OPAM 2.0
- migration to Dune 1.4 (for better handling of Menhir)
- handle SIGTERM properly
- feature #993: add BMC mode
- bug #978: handle nested "always" in INVAR field

### 0.1.7 (2018-05-23)
- remove ocamlfind from dependencies
- handle child termination

### 0.1.6 (2018-05-09)
- fix opam problems... (sigh!)

### 0.1.5 (2018-05-09)
- fix lexical error on SMV trace parsing (GH#2)
- use short names when generating SMV files (#805)
- switch to jbuilder/dune & get rid of most ppx'es
- use hashconsing internally (improved compilation speed)
- fix parsing bug of parenthesized integer expressions

### 0.1.4 (2018-01-25)
- Fix OPAM problem. '+' sign poses a problem with Github releases or topkg??

### 0.1.3+2 (2018-01-25)
- Fix OPAM problem.

### 0.1.3 (2018-01-24)
- Fix OPAM problem.

### 0.1.2 (2018-01-24)
- First OPAM (tentative) release
- Joint switch to Jbuilder/Dune

