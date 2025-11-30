#!/usr/bin/env bash

set -e

if [ $# -lt 1 ]; then
    echo "TARGET directory required"
    exit 1
fi

TARGET_DIR="$(realpath "$1")"
SOURCE_DIR="$(realpath "${BASH_SOURCE[0]%/*}")"

ln -sf \
   "$SOURCE_DIR/init.el" \
   "$SOURCE_DIR/init-mini.el" \
   "$SOURCE_DIR/early-init.el" \
   "$SOURCE_DIR/pew" \
   "$TARGET_DIR"

cat <<EOF > "$TARGET_DIR/run.sh"
emacs --init-directory='$TARGET_DIR' --debug-init "\$@"
EOF

chmod 755 "$TARGET_DIR/run.sh"
