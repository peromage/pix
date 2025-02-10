#!/usr/bin/env bash

dconf dump /org/gnome/ | dconf2nix -r /org/gnome/ > dconf.nix
