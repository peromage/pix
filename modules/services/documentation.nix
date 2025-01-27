{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf;

  cfg = config.pix.services.documentation;

in {
  options.pix.services.documentation = {
    enable = mkEnableOption "documentation generation";
  };

  config = mkIf cfg.enable {
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
