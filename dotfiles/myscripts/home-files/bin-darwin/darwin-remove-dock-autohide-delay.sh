#!/usr/bin/env bash

set -e

REVERT_TO_DEFAULT=0

while getopts "d" opt "$@"; do
    case "$opt" in
        (d) REVERT_TO_DEFAULT=1 ;;
        (*) exit 1 ;;
    esac
done

if [[ $REVERT_TO_DEFAULT -gt 0 ]]; then
    options=(autohide-delay autohide-time-modifier)
    for opt in "${options[@]}"; do
        if defaults read com.apple.dock "$opt" 1>/dev/null 2>&1; then
            defaults delete com.apple.dock "$opt"
        fi
    done
    killall Dock
    echo "Reverted dock autohide delay to the default value"
else
    defaults write com.apple.dock autohide-delay -float 0
    killall Dock
    echo "Removed dock autohide delay"
fi
