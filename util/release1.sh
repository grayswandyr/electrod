#!/bin/bash
# run from root of project repo

# $1 contains the version number to create

set -e

git flow release start $1

# clean & update headers
bash util/headache/headache.sh

# prepare changelog
DATE=`date -Idate`
echo "### $1 ($DATE)" | cat - CHANGES.md > temp && mv temp CHANGES.md

echo "Now update CHANGES.md out of this (and then test \
		 a little bit until you're happy)"
topkg status

topkg log commit

git push origin

git push github-public

topkg tag

git push --tags origin

git push --tags github-public

topkg bistro

echo "See PR in browser"
