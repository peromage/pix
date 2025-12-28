{ config, pkgs, lib, pix, ... }:

{
  imports = with pix.inputs; [
    nixpkgs.nixosModules.notDetected
    nixos-hardware.nixosModules.framework-12th-gen-intel
  ];

  boot = {
    initrd = {
      systemd.enable = true;

      availableKernelModules = [
        "xhci_pci"
        "thunderbolt"
        "nvme"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];

      kernelModules = [
        "tpm"
        "tpm_crb"
        "tpm_tis"
      ];
    };

    kernelModules = [
      "kvm-intel"
    ];

    kernelParams = [
      "nvme.noacpi=1" # Sleep power reduction
    ];

    extraModulePackages = [ ];
  };

  environment.systemPackages = with pkgs; [
    tpm2-tss
  ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  pix.hardware = {
    platform = "x86_64-linux";
    bootloader.lanzaboote.enable = true;
    networking.enable = true;
    bluetooth.enable = true;
    firmware.enable = true;
    peripherals = {
      enable = true;
      devices = [
        "printer"
        "zsa-keyboard"
        "xbox-controller"
        "smart-card"
      ];
    };
    audio.enable = true;
    power = {
      enable = true;
      profile = "powersave";
    };
  };
}
