# electrod - Formal analysis for the Electrod formal specification language

Electrod is a model finder for first-order linear temporal logic with
relations, transitive closure and partial domains.

See the file [INSTALL.md](INSTALL.md) for building and installation instructions.

[Home page](https://forge.onera.fr/projects/electrod)

## External dependencies

As of now, Electrod relies on NuSMV or nuXmv (default), so you must at least
install one of them.

## Running

Electrod is primarily aimed at being called by external, more abstract
tools, such as the [Electrum Analyzer](https://github.com/haslab/Electrum).

However, it can also be run as a standalone tool by calling the
`electrod` program.

Type `electrod --help` to get some help on options.


## Copyright and license

(C) 2016-2018 ONERA

electrod is distributed under the terms of the Mozilla Public License v2.0.

See [LICENSES/MPL-2.0.txt](LICENSES/MPL-2.0.txt) for more information.
