.PHONY: all clean doc fmt install lock opam release show-deps test utop venv watch

TARGET = electrod

DUNE = opam exec -- dune

os := $(shell opam var os)
arch := $(shell opam var arch)

RELEASE = ./$(TARGET).${os}.${arch}

SWITCH = 4.14.1

all: build

build:
	$(DUNE) build bin/$(TARGET).exe

release:
	$(DUNE) build --release bin/$(TARGET).exe

watch:
	$(DUNE) build --watch @check @fmt --auto-promote --diff-command=-

fmt:
	$(DUNE) build @fmt --auto-promote --diff-command=- || true

test:
	$(DUNE) test src

regression:
	$(DUNE) build @regression

utop:
	$(DUNE) utop --profile release

$(TARGET).opam: dune-project
	$(DUNE) build $(TARGET).opam

opam: $(TARGET).opam

lock: $(TARGET).opam
	opam lock ./$(TARGET).opam

dev-setup: 
	opam switch create --locked --yes --deps-only --with-test . 

setup: 
	opam switch create --yes --deps-only --with-test . 
	
show-deps:
	$(DUNE) external-lib-deps --missing @install

clean:
	$(DUNE) clean
	-rm -f ./$(TARGET) electrod.install

