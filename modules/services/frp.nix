{ config, lib, ... }:

let
  cfg = config.pix.services.frp;

  allowedPorts = cfg.openPorts ++ [ cfg.bindPort ];
  proxyBindAddr = if null == cfg.proxyBindAddr then cfg.bindAddr else cfg.proxyBindAddr;

in {
  options.pix.services.frp = {
    enable = lib.mkEnableOption "FRP server";

    bindAddr = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "The address that the server listens to.";
    };

    bindPort = lib.mkOption {
      type = lib.types.port;
      default = 7000;
      description = "The port for server and clients communication.";
    };

    openPorts = lib.mkOption {
      type = with lib.types; listOf port;
      default = [];
      description = "Additional ports that are allowed by firewall.";
    };

    proxyBindAddr = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = "Proxy listen address.  Default to bindAddr if null.";
    };

    password = lib.mkOption {
      type = lib.types.str;
      default = "P@55w0rd";
      description = "Token for client connection authentication.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.frp = {
      enable = true;
      role = "server";
      settings = {
        bindAddr = cfg.bindAddr;
        bindPort = cfg.bindPort;
        proxyBindAddr = proxyBindAddr;
        auth.method = "token";
        auth.token = cfg.password;
        maxPortsPerClient = 0;
      };
    };

    pix.services.firewall = {
      allowedTCPPorts = allowedPorts;
      allowedUDPPorts = allowedPorts;
    };
  };
}
