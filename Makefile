.PHONY: all clean doc fmt install lock opam release show-deps test utop venv watch

TARGET = electrod

os := $(shell opam var os)
arch := $(shell opam var arch)

RELEASE = ./$(TARGET).${os}.${arch}

SWITCH = 4.14.1

all: $(TARGET).exe

$(TARGET).exe:
	opam exec -- dune build bin/$(TARGET).exe

release:
	opam exec -- dune build --release bin/$(TARGET).exe

watch:
	opam exec -- dune build --watch @check @fmt --auto-promote --diff-command=-

fmt:
	opam exec -- dune build @fmt --auto-promote --diff-command=- || true

test:
	opam exec -- dune build @regression

utop:
	opam exec -- dune utop --profile release

doc:
	opam exec -- dune build @doc && x-www-browser _build/default/_doc/_html/index.html

$(TARGET).opam: dune-project
	opam exec -- dune build $(TARGET).opam

opam: $(TARGET).opam

lock: $(TARGET).opam
	opam lock ./$(TARGET).opam

dev-setup: 
	opam switch create --locked --yes --deps-only --with-test --with-doc . 
	
show-deps:
	opam exec -- dune external-lib-deps --missing @install

clean:
	opam exec -- dune clean
	-git clean -dfxq -e _opam
	-rm -f ./$(TARGET) electrod.install

#include $(shell ocamlfind query visitors)/Makefile.preprocess
