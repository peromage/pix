{ config, lib, ... }:

let
  cfg = config.pix.services.documentation;

in {
  options.pix.services.documentation = {
    enable = lib.mkEnableOption "documentation generation";
  };

  config = lib.mkIf cfg.enable {
    documentation = {
      enable = true;
      man.enable = true;
      info.enable = true;
      doc.enable = true;
      dev.enable = true;
      nixos = {
        enable = true;
        includeAllModules = true;
      };
    };
  };
}
