.PHONY: all clean utop test doc show-deps install uninstall

DUNE = dune

TARGET = electrod

all: build

build:
	$(DUNE) build @install \
	&& ln -sf _build/install/default/bin/$(TARGET) ./$(TARGET)

watch:
	$(DUNE) build --watch @install

test:
	$(DUNE) runtest 

utop:
	$(DUNE) utop 

doc:
	$(DUNE) build @doc && x-www-browser _build/default/_doc/_html/index.html

show-deps:
	$(DUNE) external-lib-deps --missing @install

install: build
	@$(DUNE) install

uninstall:
	@$(DUNE) uninstall

clean:
	@$(DUNE) clean
	@git clean -dfXq
	@rm -f ./$(TARGET)

include $(shell ocamlfind query visitors)/Makefile.preprocess
