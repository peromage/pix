{ config, lib, ... }:

let
  cfg = config.pix.services.sshd;

in {
  options.pix.services.sshd = {
    enable = lib.mkEnableOption "SSH service";

    enablePassword = lib.mkEnableOption "SSH password login";

    enableOnDemandActivation = lib.mkEnableOption "Only start service when there is incoming connection";

    ports = lib.mkOption {
      type = with lib.types; listOf port;
      default = [ 22 ];
      description = "SSH daemon listening ports.";
    };
  };

  config = lib.mkIf cfg.enable {
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
