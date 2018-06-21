.PHONY: all clean utop test doc show-deps install uninstall

DUNE = jbuilder

TARGET = electrod

all: build

build:
	$(DUNE) build @install --dev \
	&& ln -sf _build/install/default/bin/$(TARGET) ./$(TARGET)

watch:
	while find src/ -print0 | \
		xargs -0 inotifywait -e delete_self -e modify ;\
	do \
		make ; \
	done

test:
	$(DUNE) runtest src

utop:
	$(DUNE) utop src

doc:
	$(DUNE) build @doc && x-www-browser _build/default/_doc/_html/index.html

show-deps:
	$(DUNE) external-lib-deps --missing --dev @install

install: build
	@$(DUNE) install

uninstall:
	@$(DUNE) uninstall

clean:
	@$(DUNE) clean
	@git clean -dfXq
	@rm -f ./$(TARGET)

include $(shell ocamlfind query visitors)/Makefile.preprocess
