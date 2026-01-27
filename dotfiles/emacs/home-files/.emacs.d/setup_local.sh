#!/usr/bin/env bash

set -e

if [[ $# -lt 1 ]]; then
    echo "TARGET directory required"
    exit 1
fi

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

for i in "${REQUIRED[@]}"; do
    ln -sf "$SOURCE_DIR/$i" "$TARGET_DIR/"
done

for i in "${OPTIONAL[@]}"; do
    if [[ -e "$SOURCE_DIR/$i" ]]; then
        ln -sf "$SOURCE_DIR/$i" "$TARGET_DIR/"
    fi
done

cat <<EOF > "$TARGET_DIR/run.sh"
emacs --init-directory='$TARGET_DIR' --debug-init "\$@"
EOF

chmod 755 "$TARGET_DIR/run.sh"
