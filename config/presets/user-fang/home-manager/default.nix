{ ... }:

{
  imports = [ ./packages.nix ];

  home = {
    username = "fang";
    homeDirectory = "/home/fang";

    /*
     Alternatively source in a manual way:
     ~/.nix-profile/etc/profile.d/hm-session-vars.sh
     or
     /etc/profiles/per-user/fang/etc/profile.d/hm-session-vars.sh
    */
    sessionVariables = {
      EDITOR = "vim";
    };

    sessionPath = [
      "\${HOME}/bin"
      "\${HOME}/.local/bin"
    ];
  };
}
