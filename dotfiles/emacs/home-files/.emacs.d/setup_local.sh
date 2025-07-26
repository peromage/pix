#!/usr/bin/env bash

set -e

[ $# -lt 1 ] && exit 1

TARGET_DIR="$(realpath -L "$1")"
SOURCE_DIR="$(realpath -L "${BASH_SOURCE[0]%/*}")"

ln -sf -t "$TARGET_DIR" \
   "$SOURCE_DIR/init.el" \
   "$SOURCE_DIR/init-mini.el" \
   "$SOURCE_DIR/early-init.el" \
   "$SOURCE_DIR/pew"

cat <<EOF > "$TARGET_DIR/run.sh"
emacs --init-directory='$TARGET_DIR' --debug-init "\$@"
EOF

chmod 755 "$TARGET_DIR/run.sh"
