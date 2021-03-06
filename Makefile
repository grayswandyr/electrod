.PHONY: all clean utop test doc show-deps install uninstall regression

TARGET = electrod

os := $(shell opam var os)
arch := $(shell opam var arch)

RELEASE = ./$(TARGET).${os}.${arch}

opam_switch = 4.11.1

all: build

build:
	dune build src/$(TARGET).exe

watch:
	dune build --watch @check @fmt --auto-promote --diff-command=-

# generate opam file (in particular)
opam:
	dune build $(TARGET).opam

fmt:
	@dune build @fmt --auto-promote --diff-command=- || true

regression:
	dune build @regression

utop:
	dune utop --profile release

doc:
	dune build @doc && x-www-browser _build/default/_doc/_html/index.html

deps: opam

$(opam_file): dune-project
	-dune build @install        # Update the $(project_name).opam file
	-git add $(opam_file)       # opam uses the state of master for it updates
	-git commit $(opam_file) -m "Updating package dependencies"
	opam install . --deps-only  # Install the new dependencies

switch: deps
	opam switch create . $(opam_switch) --deps-only
	
show-deps:
	dune external-lib-deps --missing @install

clean:
	@dune clean
	@git clean -dfXq
	@rm -f ./$(TARGET) electrod.install

include $(shell ocamlfind query visitors)/Makefile.preprocess
