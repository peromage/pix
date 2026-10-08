#!/usr/bin/env bash

set -e

## Otherwise, use a UI dialog
## NOTE: Will fall back to curses if $DISPLAY is not available.

## A list of pinentry executables
## NOTE: The order matters
PROGS=(pinentry-mac pinentry-gnome3 pinentry-gtk-2 pinentry-qt pinentry-curses pinentry)
exe=

for i in "${PROGS[@]}"; do
    exe="$(command -v $i)" && break
done

if [[ -z "$exe" ]]; then
    echo "No pinentry found" >&2
    exit 1
fi

while pgrep gpg-agent >&2; do
    pkill gpg-agent >/dev/null 2>&1
done

echo "Starting new gpg-agent instance"
exec gpg-agent --pinentry-program=$exe --daemon
