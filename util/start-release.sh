#!/bin/bash
# run from root of project repo

# $1 contains the version number to create

set -e

git flow release start $1

# for a git flow release of the form release/0.x.x
VERSION=`git rev-parse --abbrev-ref HEAD | cut -f 2- -d '/'`

# update opam package version
sed -i "s/^version:.*\$/version: \"$VERSION\"/" electrod.opam

# update version string for Electrod
echo "(v. $VERSION)" > res/version

# clean & update headers
make release


