/* Settings while traveling to some restricted regions.

   For flakes the following options can be added to avoid problems during
   evaluation.

   For example:

   {
     nixConfig = {
       substituters = [
         "https://mirror.sjtu.edu.cn/nix-channels/store"
       ];
     };
   }
*/

{ config, lib, ... }:

let
  cfg = config.pix.services.traveling;

in {
  options.pix.services.traveling = {
    region = lib.mkOption {
      type = with lib.types; nullOr (enum [ "China" ]);
      default = null;
      description = "Travel region.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf ("China" == cfg.region) {
      time.timeZone = lib.mkForce "Asia/Shanghai";
      nix.settings.substituters = lib.mkForce [
        # "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
        # "https://mirrors.ustc.edu.cn/nix-channels/store"
        "https://mirror.sjtu.edu.cn/nix-channels/store"
      ];
    })
  ];
}
