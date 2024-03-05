# electrod - Formal analysis for the Electrod formal specification language

Electrod is a model finder that takes as input a
model expressed in a mixture of relational first-order logic (RFOL)
over bounded domains and linear temporal logic (LTL) over an unbounded
time horizon. Then it compiles the model to a problem for a solver 
(currently the NuSMV and nuXmv model-checkers). Electrod is mainly meant 
to be used as a backend for the [Alloy Analyzer 6](https://alloytools.org/).

[Github homepage](https://github.com/grayswandyr/electrod)

## Installation instructions

Installation has essentially been tested on GNU/Linux and Mac OS X, but it can be built for Windows.

The easiest way to install Electrod is to rely on [Opam](https://opam.ocaml.org/), the OCaml package
manager. 

Once Opam is installed and initialized, clone this directory, `cd` into it and issue the following commands:
```
make setup
make release
```
which produces a self-contained executable `electrod.exe` in the directory.

## Developement instructions

To get a working developement setup, issue the following command rather than those above:
```
make dev-setup
```
This will create an Opam local switch that provides you with a development environment 
known to be working. As usual with Opam, you must always ensure that the local switch 
is taken into account when developing (e.g. by typing `eval $(opam env)`).

To build Electrod in development mode, just type `make`.

To work in development mode, using `make watch` is suggested: it watches the source and, each time it's updated, 
checks it quickly and reformats it using `ocamlformat`.

## External dependencies

As of now, Electrod relies on NuSMV (2.6+) or nuXmv (2.0+),
so you must at least install one of them. 


## Running

Electrod is primarily aimed at being called by external tools such as the Electrum Analyzer. 

However, it can also be run as a standalone tool by calling the `electrod` program.  
Type `electrod --help` to get some help on options.


## Copyright and license

(C) 2016-2024 ONERA

electrod is distributed under the terms of the Mozilla Public License v2.0.

See [LICENSE.md](LICENSE.md) for more information.
