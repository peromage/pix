{ config, lib, ... }:

let
  cfg = config.pix.services.virtmanager;

in {
  options.pix.services.virtmanager = {
    enable = lib.mkEnableOption "virtual manager";
    enableIntelSRIOV = lib.mkEnableOption "Intel SR-IOV";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      virtualisation.libvirtd = {
        enable = true;
        onBoot = "ignore";
        onShutdown = "suspend";
      };
      programs.virt-manager.enable = true;
    })

    /*
       FIXME: Missing a DKMS module:
       https://github.com/strongtz/i915-sriov-dkms
    */
    (lib.mkIf (cfg.enable && cfg.enableIntelSRIOV) {
      boot = {
        kernelParams = [
          "intel_iommu=on"
          "i915.enable_guc=3"
          "i915.max_vfs=7"
        ];

        kernelModules = [
          "i915"
        ];
      };
    })
  ];
}
