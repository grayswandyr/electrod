
TESTDIR=test

CHECK_INSTALL = ocamlfind query

OCB_FLAGS = -use-ocamlfind -j 0 #-classic-display

OCB = ocamlbuild $(OCB_FLAGS)

HTML_FLAGS = -docflags "-safe-string,-short-paths,-sort,-colorize-code,\
	-short-functors,-css-style ../doc/style.css,-charset utf-8,-t Electrod,-intro ../../dot.html"

MAIN = electrod

all: byte 

clean:
	$(OCB) -clean
	$(OCB) -clean -build-dir _coverage
	rm bisect*.out run_tests.qtestpack doc/intro.text 2> /dev/null || true

native: 
	$(OCB) $(MAIN).native

release: clean native


byte: 
	$(OCB) $(MAIN).byte

profile: 
	$(OCB) -tag profile $(MAIN).native

landmarks:
	$(OCB) -pkg landmarks.ppx -pkg landmarks $(MAIN).native

test: test-requisites
	ocamldep -one-line -all -sort src/*.ml | sed 's/.ml//g' | sed 's/src\///g' \
	| sed 's/electrod//' | sed 's/Main//'> ./run_tests.qtestpack
	$(OCB) -pkgs bisect_ppx,oUnit,qcheck run_tests.byte
	./run_tests.byte
	bisect-ppx-report -I _build -html _coverage/ bisect*.out

doc: all doc-requisites
# create odocl file by listing all ml or mli files and keeping one name for each
	@find src/ -name '*.ml' -printf "%f\n" | grep -v  '#' | xargs -r basename -s .ml > doc/api.odocl
	@find src/ -name '*.mli' -printf "%f\n" | grep -v  '#' | xargs -r basename -s .mli >> doc/api.odocl
	@find src/ -name '*.mll' -printf "%f\n" | grep -v  '#' | xargs -r basename -s .mll >> doc/api.odocl
	@find src/ -name '*.mly' -printf "%f\n" | grep -v  '#' | xargs -r basename -s .mly >> doc/api.odocl
# uppercase the first letter of every name (to get OCaml module names)
	@sed -i 's/.*/\u&/' doc/api.odocl
	@sort -u doc/api.odocl -o doc/api.odocl
# create index page of ocamldoc with list of all modules and pictures for module and type dependencies
	@cat doc/intro.base > doc/intro.text
	@echo '<h2>Modules</h2>{!modules:' >> doc/intro.text
	@cat	doc/api.odocl >> doc/intro.text
	@echo '}{%html:<h2>Module dependencies</h2><img src="modules.svg" width="300%">' >> doc/intro.text
	@echo '<h2>Type dependencies</h2><img src="types.svg" width="300%">%}' >> doc/intro.text
# generate documentation
	@$(OCB) $(HTML_FLAGS) doc/api.docdir/index.html 
	@$(OCB) -docflag -dot-reduce doc/api.docdir/modules.dot
	@$(OCB) -docflags '-dot-types,-dot-reduce' doc/api.docdir/types.dot
# remove unnecessary files (now that the doc has been created)
	@rm doc/intro.text
# ocamldoc -dot adds a rotation in dot files: remove it; then create svg files
	@sed -i 's/rotate.*//' api.docdir/modules.dot
	@sed -i 's/rotate.*//' api.docdir/types.dot
	@dot -Tsvg api.docdir/modules.dot > api.docdir/modules.svg
	@dot -Tsvg api.docdir/types.dot > api.docdir/types.svg


test-requisites:
	@which sed > /dev/null

doc-requisites:
	@which echo > /dev/null
	@which sed > /dev/null
	@which dot > /dev/null
	@which xargs > /dev/null
	@which find > /dev/null
	@which basename > /dev/null
	@which sort > /dev/null

check-config:
	@which ocamlfind > /dev/null
	@which ocamlbuild > /dev/null
	@$(CHECK_INSTALL) cmdliner containers gen sequence logs logs.fmt \
	logs.cli fmt fmt.tty fmt.cli menhirLib mtime.os ppx_deriving bisect_ppx \
	visitors hashcons > /dev/null


.PHONY: all clean byte native profile debug check-config test doc doc-requisites test-requisites

include $(shell ocamlfind query visitors)/Makefile.preprocess
