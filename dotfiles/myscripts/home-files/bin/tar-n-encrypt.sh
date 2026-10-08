#!/usr/bin/env bash

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 TARGET"
    exit 1
fi

TARGET="$(realpath $1)"
TARGET_BASE="$(basename $TARGET)"
WORK_DIR="$(dirname "$TARGET")"

tar -C "$WORK_DIR" -cv "$TARGET_BASE" | gpg -c -a -o "${TARGET_BASE}-$(date +%Y%m%d-%H%M%S).tar.asc"
