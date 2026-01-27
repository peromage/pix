#!/usr/bin/env bash

set -e

function help_and_exit {
    cat <<EOF
Usage:
  $(basename $0) [-r | -o] DIR

Options:
  -r    Set up only required files
  -o    Set up only optional files

By default, if neither '-r' nor '-o' is specified, both required and optional
files are copied.
EOF
}

while getopts "ro" opt "$@"; do
    case "$opt" in
        (r) SETUP_REQUIRED=1 ;;
        (o) SETUP_OPTIONAL=1 ;;
        (*) help_and_exit ;;
    esac
done

shift $((OPTIND - 1))

TARGET_DIR="$(realpath "$1")"
SOURCE_DIR="$(realpath "${BASH_SOURCE[0]%/*}")"

REQUIRED=(
    init.el
    init-mini.el
    early-init.el
    pew
)

OPTIONAL=(
    straight
    tree-sitter
)

function setup_required {
    for i in "${REQUIRED[@]}"; do
        ln -sf "$SOURCE_DIR/$i" "$TARGET_DIR/"
    done
}

function setup_optional {
    for i in "${OPTIONAL[@]}"; do
        if [[ -e "$SOURCE_DIR/$i" ]]; then
            ln -sf "$SOURCE_DIR/$i" "$TARGET_DIR/"
        fi
    done
}

[[ "$SETUP_REQUIRED" -eq 1 ]] && setup_required
[[ "$SETUP_OPTIONAL" -eq 1 ]] && setup_optional

if [[ -z "$SETUP_REQUIRED" ]] && [[ -z "$SETUP_OPTIONAL" ]]; then
    setup_required
    setup_optional
fi

cat <<EOF > "$TARGET_DIR/run.sh"
emacs --init-directory='$TARGET_DIR' --debug-init "\$@"
EOF

chmod 755 "$TARGET_DIR/run.sh"
