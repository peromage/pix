{
  lib,
  config,
  pix,
  ...
}: let
  cfg = config.pix.machines.nuc;
  mkRequiredOption = pix.lib.mkRequiredOption;
in {
  # Some options containing secrets must be set on local before evaluation
  options.pix.machines.nuc = with lib.types; {
    wangguanPassword = mkRequiredOption str "";

    sshdPorts = mkRequiredOption (listOf port) "";

    shadowsocksPassword = mkRequiredOption str "";
    shadowsocksPort = mkRequiredOption port "";
  };
  config = {
    pix.system.immutableUsers = true;
    pix.users.wangguan.password = cfg.wangguanPassword;

    # Services
    pix.services.sshd.ports = cfg.sshdPorts;

    pix.services.shadowsocks = {
      enable = true;
      port = cfg.shadowsocksPort;
      password = cfg.shadowsocksPassword;
      extraConfig = {
        nameserver = "1.1.1.1,1.0.0.1";
      };
    };

    # pix.services.frp = {
    #   enable = true;
    #   bindPort = cfg.frpPort;
    #   proxyBindAddr = cfg.frpBindAddr;
    #   openPorts = cfg.frpOpenPorts;
    #   password = cfg.frpPassword;
    # };
  };
}
