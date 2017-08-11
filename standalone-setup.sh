#!/usr/bin/env bash

################################################################################
# Standalone repository setup
#
# By default, the Makefiles and Stack files in this repository assume
# that it has been checked out as a submodule of the
# `smaccmpilot-build` repository. For working with this repository in
# isolation, such as when testing on Travis-CI, this script will check
# out dependencies in this repository and change the Stack files
# appropriately.
################################################################################

# First, check out depenendency repos, then check out the branch that
# matches the current branch name in this repo. If one doesn't exist,
# don't fail, in case we're branched in this repo but just depending
# on master elsewhere.
BRANCH=${TRAVIS_BRANCH:-$(git symbolic-ref --short HEAD)}
SUBREPOS="ivory tower ivory-tower-stm32 ivory-tower-posix tower-camkes-odroid gidl gec"
for repo in $SUBREPOS;
do
    git clone https://github.com/galoisinc/$repo
    (cd $repo; git checkout $BRANCH) || true
done

# Second, we have to adjust the paths in the various stack
# files. Since Stack does not yet support templating, this means we
# must substitute the `../` with the current directory, so that the
# repositories checked out above can be found.

STACKFILES="stack-7.8.yaml stack.yaml stack-8.0.2.yaml gidl-bootstrap.yaml"
for stackfile in $STACKFILES;
do
    sed -i.orig "s;\.\./;$PWD/;g" $stackfile
done
