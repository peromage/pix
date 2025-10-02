# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/Connections" = {
      first-run = false;
    };

    "org/gnome/Console" = {
      custom-font = "Iosevka 10";
      font-scale = 1.2000000000000002;
      last-window-maximised = false;
      last-window-size = mkTuple [ 1210 834 ];
      use-system-font = false;
    };

    "org/gnome/Extensions" = {
      window-height = 1011;
      window-maximized = false;
      window-width = 1209;
    };

    "org/gnome/Geary" = {
      migrated-config = true;
    };

    "org/gnome/Loupe" = {
      show-properties = false;
    };

    "org/gnome/Snapshot" = {
      is-maximized = false;
      window-height = 640;
      window-width = 800;
    };

    "org/gnome/TextEditor" = {
      highlight-current-line = true;
      last-save-directory = "file:///home/fang/Desktop";
      restore-session = true;
      show-grid = false;
      show-map = true;
    };

    "org/gnome/Totem" = {
      active-plugins = [ "vimeo" "variable-rate" "skipto" "screenshot" "screensaver" "save-file" "rotation" "recent" "movie-properties" "open-directory" "mpris" "autoload-subtitles" "apple-trailers" ];
      subtitle-encoding = "UTF-8";
    };

    "org/gnome/calculator" = {
      accuracy = 9;
      angle-units = "degrees";
      base = 10;
      button-mode = "basic";
      number-format = "automatic";
      show-thousands = false;
      show-zeroes = false;
      source-currency = "";
      source-units = "degree";
      target-currency = "";
      target-units = "radian";
      window-maximized = false;
      window-size = mkTuple [ 367 576 ];
      word-size = 64;
    };

    "org/gnome/calendar" = {
      active-view = "month";
      window-maximized = true;
      window-size = mkTuple [ 768 600 ];
    };

    "org/gnome/clocks/state/window" = {
      maximized = false;
      panel-id = "stopwatch";
      size = mkTuple [ 870 690 ];
    };

    "org/gnome/control-center" = {
      last-panel = "keyboard";
      window-state = mkTuple [ 1275 747 false ];
    };

    "org/gnome/desktop/a11y/applications" = {
      screen-keyboard-enabled = false;
      screen-magnifier-enabled = false;
    };

    "org/gnome/desktop/app-folders" = {
      folder-children = [ "Utilities" "YaST" ];
    };

    "org/gnome/desktop/app-folders/folders/Utilities" = {
      apps = [ "gnome-abrt.desktop" "gnome-system-log.desktop" "nm-connection-editor.desktop" "org.gnome.baobab.desktop" "org.gnome.Connections.desktop" "org.gnome.DejaDup.desktop" "org.gnome.Dictionary.desktop" "org.gnome.DiskUtility.desktop" "org.gnome.eog.desktop" "org.gnome.Evince.desktop" "org.gnome.FileRoller.desktop" "org.gnome.fonts.desktop" "org.gnome.seahorse.Application.desktop" "org.gnome.tweaks.desktop" "org.gnome.Usage.desktop" "vinagre.desktop" ];
      categories = [ "X-GNOME-Utilities" ];
      name = "X-GNOME-Utilities.directory";
      translate = true;
    };

    "org/gnome/desktop/app-folders/folders/YaST" = {
      categories = [ "X-SuSE-YaST" ];
      name = "suse-yast.directory";
      translate = true;
    };

    "org/gnome/desktop/background" = {
      color-shading-type = "solid";
      picture-options = "scaled";
      picture-uri = "file:///home/fang/Pictures/Arknights_Wallpaper/%E5%AE%A3%E4%BC%A0%E5%9B%BE/%E6%B0%B4%E6%99%B6%E7%AE%AD%E8%A1%8C%E5%8A%A8.jpg";
      picture-uri-dark = "file:///home/fang/Pictures/Arknights_Wallpaper/%E5%AE%A3%E4%BC%A0%E5%9B%BE/%E6%B0%B4%E6%99%B6%E7%AE%AD%E8%A1%8C%E5%8A%A8.jpg";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
    };

    "org/gnome/desktop/calendar" = {
      show-weekdate = false;
    };

    "org/gnome/desktop/datetime" = {
      automatic-timezone = false;
    };

    "org/gnome/desktop/input-sources" = {
      mru-sources = [ (mkTuple [ "xkb" "us" ]) ];
      sources = [ (mkTuple [ "xkb" "us" ]) ];
      xkb-options = [ "terminate:ctrl_alt_bksp" "lv3:ralt_switch" ];
    };

    "org/gnome/desktop/interface" = {
      clock-format = "24h";
      clock-show-date = true;
      clock-show-weekday = false;
      color-scheme = "prefer-dark";
      cursor-size = 48;
      locate-pointer = false;
      show-battery-percentage = true;
      text-scaling-factor = 1.25;
      toolkit-accessibility = false;
    };

    "org/gnome/desktop/notifications" = {
      application-children = [ "gnome-power-panel" "org-gnome-console" "brave-browser" "org-gnome-settings" "gnome-network-panel" "org-gnome-nautilus" "firefox" "balena-etcher-electron" "org-gnome-fileroller" "emacsclient" "org-wezfurlong-wezterm" "steam" "qq" "gimp" "org-gnome-texteditor" "discord" "com-discordapp-discord" "com-valvesoftware-steam" "minecraft-launcher" "org-gnome-software" "org-gnome-terminal" "org-prismlauncher-prismlauncher" ];
    };

    "org/gnome/desktop/notifications/application/balena-etcher-electron" = {
      application-id = "balena-etcher-electron.desktop";
    };

    "org/gnome/desktop/notifications/application/brave-browser" = {
      application-id = "brave-browser.desktop";
    };

    "org/gnome/desktop/notifications/application/com-discordapp-discord" = {
      application-id = "com.discordapp.Discord.desktop";
    };

    "org/gnome/desktop/notifications/application/com-valvesoftware-steam" = {
      application-id = "com.valvesoftware.Steam.desktop";
    };

    "org/gnome/desktop/notifications/application/discord" = {
      application-id = "discord.desktop";
    };

    "org/gnome/desktop/notifications/application/emacsclient" = {
      application-id = "emacsclient.desktop";
    };

    "org/gnome/desktop/notifications/application/firefox" = {
      application-id = "firefox.desktop";
    };

    "org/gnome/desktop/notifications/application/gimp" = {
      application-id = "gimp.desktop";
    };

    "org/gnome/desktop/notifications/application/gnome-network-panel" = {
      application-id = "gnome-network-panel.desktop";
    };

    "org/gnome/desktop/notifications/application/gnome-power-panel" = {
      application-id = "gnome-power-panel.desktop";
    };

    "org/gnome/desktop/notifications/application/minecraft-launcher" = {
      application-id = "minecraft-launcher.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-console" = {
      application-id = "org.gnome.Console.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-fileroller" = {
      application-id = "org.gnome.FileRoller.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-nautilus" = {
      application-id = "org.gnome.Nautilus.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-settings" = {
      application-id = "org.gnome.Settings.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-software" = {
      application-id = "org.gnome.Software.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-terminal" = {
      application-id = "org.gnome.Terminal.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-texteditor" = {
      application-id = "org.gnome.TextEditor.desktop";
    };

    "org/gnome/desktop/notifications/application/org-prismlauncher-prismlauncher" = {
      application-id = "org.prismlauncher.PrismLauncher.desktop";
    };

    "org/gnome/desktop/notifications/application/org-wezfurlong-wezterm" = {
      application-id = "org.wezfurlong.wezterm.desktop";
    };

    "org/gnome/desktop/notifications/application/qq" = {
      application-id = "QQ.desktop";
    };

    "org/gnome/desktop/notifications/application/steam" = {
      application-id = "steam.desktop";
    };

    "org/gnome/desktop/peripherals/keyboard" = {
      delay = mkUint32 227;
      numlock-state = true;
    };

    "org/gnome/desktop/peripherals/mouse" = {
      accel-profile = "default";
      left-handed = false;
      natural-scroll = false;
      speed = 0.0;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      click-method = "areas";
      speed = 0.1428571428571428;
      tap-to-click = false;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/privacy" = {
      old-files-age = mkUint32 30;
      recent-files-max-age = -1;
    };

    "org/gnome/desktop/screensaver" = {
      color-shading-type = "solid";
      picture-options = "zoom";
      picture-uri = "file:///home/fang/.local/share/backgrounds/2025-02-07-09-46-50-%E6%B3%B0%E6%8B%89%E9%A5%AD.jpg";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
    };

    "org/gnome/desktop/search-providers" = {
      sort-order = [ "org.gnome.Contacts.desktop" "org.gnome.Documents.desktop" "org.gnome.Nautilus.desktop" ];
    };

    "org/gnome/desktop/session" = {
      idle-delay = mkUint32 600;
    };

    "org/gnome/desktop/sound" = {
      event-sounds = true;
      theme-name = "__custom";
    };

    "org/gnome/desktop/wm/keybindings" = {
      begin-resize = [];
      cycle-group = [];
      cycle-group-backward = [];
      cycle-panels = [];
      cycle-panels-backward = [];
      cycle-windows = [];
      cycle-windows-backward = [];
      maximize-horizontally = [ "<Shift><Super>Left" ];
      maximize-vertically = [ "<Shift><Super>Up" ];
      minimize = [ "<Super>d" ];
      move-to-monitor-down = [];
      move-to-monitor-left = [];
      move-to-monitor-right = [];
      move-to-monitor-up = [];
      move-to-workspace-1 = [];
      move-to-workspace-last = [];
      move-to-workspace-left = [ "<Super>comma" ];
      move-to-workspace-right = [ "<Super>period" ];
      panel-run-dialog = [ "<Super>r" ];
      switch-applications = [];
      switch-applications-backward = [];
      switch-input-source = [];
      switch-input-source-backward = [];
      switch-panels = [];
      switch-panels-backward = [];
      switch-to-workspace-1 = [];
      switch-to-workspace-last = [];
      switch-to-workspace-left = [ "<Super>b" ];
      switch-to-workspace-right = [ "<Super>f" ];
      switch-windows = [ "<Alt>Tab" ];
      switch-windows-backward = [ "<Shift><Alt>Tab" ];
      toggle-maximized = [];
    };

    "org/gnome/desktop/wm/preferences" = {
      button-layout = "appmenu:minimize,maximize,close";
      num-workspaces = 6;
    };

    "org/gnome/eog/ui" = {
      sidebar = false;
      statusbar = true;
    };

    "org/gnome/evince/default" = {
      window-ratio = mkTuple [ 0.9803921568627451 0.7575757575757576 ];
    };

    "org/gnome/evolution-data-server" = {
      migrated = true;
    };

    "org/gnome/file-roller/dialogs/extract" = {
      recreate-folders = true;
      skip-newer = false;
    };

    "org/gnome/file-roller/listing" = {
      list-mode = "as-folder";
      name-column-width = 545;
      show-path = false;
      sort-method = "name";
      sort-type = "ascending";
    };

    "org/gnome/file-roller/ui" = {
      sidebar-width = 200;
      window-height = 474;
      window-width = 1095;
    };

    "org/gnome/mutter" = {
      dynamic-workspaces = true;
      edge-tiling = true;
    };

    "org/gnome/mutter/wayland/keybindings" = {
      restore-shortcuts = [];
    };

    "org/gnome/nautilus/list-view" = {
      default-column-order = [ "name" "size" "type" "owner" "group" "permissions" "where" "date_modified" "date_modified_with_time" "date_accessed" "date_created" "recency" "detailed_type" ];
      default-visible-columns = [ "name" "size" "date_modified" "date_created" ];
    };

    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "list-view";
      migrated-gtk-settings = true;
      recursive-search = "never";
      search-filter-time-type = "last_modified";
      search-view = "list-view";
      show-create-link = true;
      show-delete-permanently = true;
    };

    "org/gnome/nautilus/window-state" = {
      initial-size = mkTuple [ 1254 911 ];
      maximized = false;
    };

    "org/gnome/nm-applet/eap/069ef1dc-5e7b-4db5-8522-c4e5f4b8f562" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/0e8d1d58-8b85-4607-87d3-9aaadd88f529" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/19053a31-67f5-4f9e-8378-43609a49430c" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/2af17933-d955-41d5-adb3-3aac734a5feb" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/2ff1f062-ce10-4a01-95de-150c7541f093" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/ac5cd930-f81a-4313-8b3c-26bf5af44fb4" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/fd16c493-51e4-4d81-b673-a2a5fd17e3d0" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/portal/filechooser/brave-browser" = {
      last-folder-path = "/home/fang/Downloads";
    };

    "org/gnome/portal/filechooser/com/microsoft/Edge" = {
      last-folder-path = "/home/fang/Documents";
    };

    "org/gnome/portal/filechooser/gnome-background-panel" = {
      last-folder-path = "/home/fang/Pictures/Arknights_Wallpaper/\23459\20256\22270";
    };

    "org/gnome/portal/filechooser/org/gnome/Settings" = {
      last-folder-path = "/home/fang/Downloads";
    };

    "org/gnome/portal/filechooser/org/gnome/tweaks" = {
      last-folder-path = "/home/fang/Pictures/Arknights_Wallpaper/\23459\20256\22270";
    };

    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = false;
      night-light-temperature = mkUint32 3293;
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/" "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/" "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/" ];
      magnifier = [];
      magnifier-zoom-in = [];
      magnifier-zoom-out = [];
      screenreader = [];
      screensaver = [ "<Super>l" ];
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Super>t";
      command = "kitty";
      name = "Kitty";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
      binding = "<Super>e";
      command = "nautilus -w";
      name = "Nautilus";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
      binding = "<Super>c";
      command = "gnome-text-editor -n";
      name = "Text Editor";
    };

    "org/gnome/settings-daemon/plugins/power" = {
      idle-dim = false;
      power-button-action = "suspend";
      sleep-inactive-ac-type = "nothing";
    };

    "org/gnome/shell" = {
      command-history = [ "r" "bash" "systemctl hibernation" "flameshot" "systemctl hibernate" "restart" "zeal" "webex" "nix run nixpkgs#barrier" "emacs" ];
      disable-user-extensions = false;
      disabled-extensions = [ "launch-new-instance@gnome-shell-extensions.gcampax.github.com" "native-window-placement@gnome-shell-extensions.gcampax.github.com" "places-menu@gnome-shell-extensions.gcampax.github.com" "screenshot-window-sizer@gnome-shell-extensions.gcampax.github.com" "light-style@gnome-shell-extensions.gcampax.github.com" "windowsNavigator@gnome-shell-extensions.gcampax.github.com" "workspace-indicator@gnome-shell-extensions.gcampax.github.com" ];
      enabled-extensions = [ "kimpanel@kde.org" "trayIconsReloaded@selfmade.pl" "apps-menu@gnome-shell-extensions.gcampax.github.com" "x11gestures@joseexposito.github.io" "window-list@gnome-shell-extensions.gcampax.github.com" ];
      favorite-apps = [ "firefox.desktop" "emacsclient.desktop" "org.gnome.Nautilus.desktop" "org.gnome.Terminal.desktop" "org.gnome.Settings.desktop" "com.microsoft.Edge.desktop" ];
      last-selected-power-profile = "power-saver";
      welcome-dialog-last-shown-version = "44.2";
    };

    "org/gnome/shell/extensions/window-list" = {
      display-all-workspaces = false;
      grouping-mode = "never";
      show-on-all-monitors = true;
    };

    "org/gnome/shell/keybindings" = {
      focus-active-notification = [];
      screenshot = [ "Print" ];
      screenshot-window = [];
      show-screen-recording-ui = [ "<Shift><Super>r" ];
      show-screenshot-ui = [ "<Super>s" ];
      toggle-message-tray = [];
      toggle-overview = [];
      toggle-quick-settings = [];
    };

    "org/gnome/shell/world-clocks" = {
      locations = [];
    };

    "org/gnome/software" = {
      check-timestamp = mkInt64 1738854237;
      first-run = false;
      flatpak-purge-timestamp = mkInt64 1738977316;
      update-notification-timestamp = mkInt64 1736292234;
    };

    "org/gnome/terminal/legacy" = {
      always-check-default-terminal = false;
      menu-accelerator-enabled = false;
      mnemonics-enabled = false;
      shortcuts-enabled = true;
    };

    "org/gnome/terminal/legacy/keybindings" = {
      close-tab = "disabled";
      close-window = "disabled";
      find = "<Primary><Shift>f";
      find-clear = "disabled";
      find-next = "disabled";
      find-previous = "disabled";
      full-screen = "disabled";
      move-tab-left = "disabled";
      move-tab-right = "disabled";
      new-tab = "<Primary><Shift>t";
      new-window = "<Primary><Shift>n";
      next-tab = "<Primary>grave";
      prev-tab = "<Primary>asciitilde";
      switch-to-tab-1 = "disabled";
      switch-to-tab-10 = "disabled";
      switch-to-tab-2 = "disabled";
      switch-to-tab-3 = "disabled";
      switch-to-tab-4 = "disabled";
      switch-to-tab-5 = "disabled";
      switch-to-tab-6 = "disabled";
      switch-to-tab-7 = "disabled";
      switch-to-tab-8 = "disabled";
      switch-to-tab-9 = "disabled";
      zoom-in = "disabled";
      zoom-normal = "disabled";
      zoom-out = "disabled";
    };

    "org/gnome/terminal/legacy/profiles:" = {
      default = "233c6191-db1e-403e-9b76-0f006019cf4c";
      list = [ "233c6191-db1e-403e-9b76-0f006019cf4c" ];
    };

    "org/gnome/terminal/legacy/profiles:/:233c6191-db1e-403e-9b76-0f006019cf4c" = {
      cursor-blink-mode = "off";
      custom-command = "fish -i";
      font = "Iosevka 12";
      text-blink-mode = "always";
      use-custom-command = true;
      use-system-font = false;
      use-theme-colors = true;
      visible-name = "my_fish";
    };

    "org/gnome/tweaks" = {
      show-extensions-notice = false;
    };

  };
}
