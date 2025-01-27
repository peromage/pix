{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    types;

  cfg = config.pix.services.sshd;

in {
  options.pix.services.sshd = {
    enable = mkEnableOption "SSH service";

    enablePassword = mkEnableOption "SSH password login";

    enableOnDemandActivation = mkEnableOption "Only start service when there is incoming connection";

    ports = mkOption {
      type = with types; listOf port;
      default = [ 22 ];
      description = "SSH daemon listening ports.";
    };
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      ports = cfg.ports;
      openFirewall = true; # Whitelist the ports
      settings = {
        X11Forwarding = false;
        PermitRootLogin = "no";
        PasswordAuthentication = cfg.enablePassword;
      };
    };
  };
}
