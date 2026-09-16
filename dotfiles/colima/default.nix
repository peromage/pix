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

    # colima tends to overwrite config files so don't link
    # See also: https://iniakunhuda.medium.com/2-years-with-colima-the-optimization-guide-i-wish-i-had-from-day-one-8b89b8155285
    home.file."COLIMA_COPY_ONLY" = {
      text = "";
      force = true; # Ensure this is always run
      onChange = ''
        rsync() { "${pkgs.rsync}/bin/rsync" "$@"; }
        rm ${homeDir}/COLIMA_COPY_ONLY
        rsync -abc --chmod=u=rw,g=rw,o=r ${src}/* ${homeDir}/.config/colima
      '';
    };
  };
}
