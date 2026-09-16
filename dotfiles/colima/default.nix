{ config, pkgs, lib, ...}:

let
  cfg = config.pix.dotfiles.colima;
  src = ./home-files/.config/colima;
  homeDir = config.home.homeDirectory;

in {
  options.pix.dotfiles.colima = {
    enable = lib.mkEnableOption "Pot Colima";
  };

  config = lib.mkIf cfg.enable {
    services.colima.enable = true;

    home = {
      # colima tends to overwrite config files so don't link
      # See also: https://iniakunhuda.medium.com/2-years-with-colima-the-optimization-guide-i-wish-i-had-from-day-one-8b89b8155285
      activation.copyColimaConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        export PATH="${pkgs.rsync}/bin:$PATH"
        run rsync -abc --chmod=D755,F644 ${src}/* ${homeDir}/.config/colima
      '';

      packages = with pkgs; [ docker ];

      sessionVariables = {
        # colima overwrite config by default. Set this env to disable it
        COLIMA_SAVE_CONFIG = 0;
      };
    };
  };
}
