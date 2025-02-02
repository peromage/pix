{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.bash;
  src = ./home;

in {
  options.pix.dotfiles.bash = {
    enable = lib.mkEnableOption "Pot Bash";
  };

  config = lib.mkIf cfg.enable {
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
