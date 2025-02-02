{ config, lib, ... }:

let
  cfg = config.pix.services.shadowsocks;

in {
  options.pix.services.shadowsocks = {
    enable = lib.mkEnableOption "ShadowSocks";

    port = lib.mkOption {
      type = with lib.types; nullOr port;
      default = 8388;
      description = "Default service port.";
    };

    bind = lib.mkOption {
      type = with lib.types; listOf str;
      default = [ "0.0.0.0" ];
      description = "Addresses to listen to.";
    };

    password = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = "Connection password.";
    };

    extraConfig = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Extra configurations in keys and values.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.shadowsocks = {
      enable = true;
      password = cfg.password;
      port = cfg.port;
      localAddress = cfg.bind;
      extraConfig = cfg.extraConfig;
      encryptionMethod = "chacha20-ietf-poly1305";
      fastOpen = true;
      mode = "tcp_and_udp";
    };

    networking.firewall.allowedTCPPorts = [ cfg.port ];

    assertions = lib.singleton {
      assertion = null != cfg.password;
      message = "No password specified for service Shadowsocks.";
    };
  };
}
