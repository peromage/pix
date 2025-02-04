#!/usr/bin/env bash

set -e

function usage {
    >&2 cat <<EOF
A simple wrapper of GNU Stow tailored for POT.

Usage:
  $0 ACTION package [package [...]]

Actions:
  -S    Stow packages
  -D    Delete packages
  -R    Restow packages

Note: Each package requires a sub-directory "home-files" which contains the dotfiles.
EOF
}

function run_stow {
    local action=$1 && shift
    for pkg in "$@"; do
        cmd="stow --dir=$pkg --target=$HOME --no-folding -v $action home-files"
        echo "> RUN: $cmd"
        eval "$cmd"
    done
}

if [[ $# -lt 2 ]]; then
    usage
    exit 1
fi

case $1 in
    (-S) ;&
    (-D) ;&
    (-R) run_stow "$@" ;;
    (-h|--help) ;&
    (*) usage
esac
