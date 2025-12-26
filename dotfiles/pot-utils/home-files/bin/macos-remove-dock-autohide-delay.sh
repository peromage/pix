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
    defaults delete com.apple.dock autohide-delay
    defaults delete com.apple.dock autohide-time-modifier
    killall Dock
else
    defaults write com.apple.dock autohide-delay -float 0
    killall Dock
fi
