/*
   Nixified Gnome dconf configurations.

   To reliably convert dumped dconf database to home-manager config, use `dconf2nix'.

   1. Make modifications through Gnome menus.
   2. Dump and nixify dconf: `dconf dump /org/gnome/ | dconf2nix -r /org/gnome/ > dconf.nix'
   3. Cherry-pick config.
*/

{ config, lib, ...}:

let
  cfg = config.pix.dotfiles.gnome;
  dconfDump = import ./dconf.nix { inherit lib; };
  getConf = regex: lib.filterAttrs (name: _: lib.match regex name != null) dconfDump.dconf.settings;
  getConfs = lib.foldl (acc: regex: acc // getConf regex) {};

in {
  options.pix.dotfiles.gnome = {
    enableKeyboardShortcuts = lib.mkEnableOption "Customized keyboard shortcut";
    enableGnomeTerminalConfig = lib.mkEnableOption "Customized Gnome Terminal config";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enableKeyboardShortcuts {
      dconf.settings = getConfs [
        "org/gnome/desktop/wm/keybindings"
        "org/gnome/mutter/wayland/keybindings"
        "org/gnome/settings-daemon/plugins/media-keys"
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/.*"
        "org/gnome/shell/keybindings"
      ];
    })

    (lib.mkIf cfg.enableGnomeTerminalConfig {
      dconf.settings = getConfs [
        "org/gnome/terminal/legacy"
        "org/gnome/terminal/legacy/keybindings"
        "org/gnome/terminal/legacy/profiles:"
        "org/gnome/terminal/legacy/profiles:/.*"
      ];
    })
  ];
}
