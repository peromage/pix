#!/usr/bin/env bash

## NOTE: This script tends to be used on MacOS only.

## Direct source from: https://github.com/craigfurman/nix-workstations/blob/fe7fa3da64425301a1f83f22475b6d3f20e6369d/home/apps/make-app-trampolines.sh
## I changed a little bit based on that.
##
## How to run this automatically on switching Home Manager profile
##
## home.activation.fixMacOSApps = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
##   run ${pot-util}/bin/fix-macos-hm-apps.sh
## ''
##
## See also: https://github.com/nix-community/home-manager/issues/1341#issuecomment-1705731962
## The author later made a Nix utility which comes handy.

set -e

SOURCE="$HOME/Applications/Home Manager Apps"
TARGET="$HOME/Applications/home-manager-trampolines"
CC=/usr/bin/osacompile

if [ ! -d "$SOURCE" ]; then
    echo "It doesn't look like a MacOS or app directory doesn't exist: $SOURCE"
    exit 1
fi

# Cleanup
rm -rf "$TARGET"
mkdir -p "$TARGET"

cd "$SOURCE"
for app in *.app; do
    /usr/bin/osacompile -o "$TARGET/$app" -e "do shell script \". /etc/profile && open '$SOURCE/$app'\""
done
