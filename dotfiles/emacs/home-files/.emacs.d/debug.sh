#!/usr/bin/env bash
### debug.sh --- Load Emacs from this dev directory instead of .emacs.d

set -e

SRC="$HOME/.emacs.d"

ITEMS=(
    straight/build
    straight/modified
    straight/repos
    straight/build-cache.el
    tree-sitter
)

show_help() {
    cat <<EOF
Usage:
  $(basename $0) [-c] -- ARGS

Options:
  -c    Copy files from existing .emacs.d directory (package files usually)

ARGS are arguments passed to emacs.
EOF
}

cd "$(dirname "${BASH_SOURCE[0]}")"

copy_from_emacs_d() {
    echo "Copying files from $SRC ..."

    for i in "${ITEMS[@]}"; do
        if [[ -e "$SRC/$i" ]]; then
            echo "Copying $SRC/$i to $i"
            rm -rf "./straight/$i"
            cp -af "$SRC/$i" "./$i"
        else
            echo "Does not exist: $SRC/$i"
        fi
    done

    echo "Done"
}

while getopts "ch" opt "$@"; do
    case "$opt" in
        c)
            copy_from_emacs_d
            exit 0
            ;;
        *) show_help
           exit 1
           ;;
    esac
done

shift $((OPTIND - 1))
emacs --init-directory="$(pwd)" --debug-init "$@"
