{ config, lib, pix, ... }:

let
  cfg = config.pix.dotfiles.bash;
  src = ./home;

in with lib; {
  options.pix.dotfiles.bash = {
    enable = mkEnableOption "Bash";
  };

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;
      enableCompletion = true;
      enableVteIntegration = true;
      bashrcExtra = "";
      profileExtra = "";
      logoutExtra = "";
      initExtra = ''
        source ${src}/.bashrc noenv
      '';
    };
  };
}
