{ pix, nixpkgs }:

{
  unrestrictedPkgs = final: prev: {
    unrestrictedPkgs = import pix.inputs.nixpkgs {
      inherit (final) system;
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
    };
  };

  pixPkgs = final: prev: {
    pixPkgs = pix.packages.${final.system};
  };

  callPackageHelpers = final: prev: {
    callPackage = prev.newScope { inherit pix; };
    callPackageAttrs = nixpkgs.lib.mapAttrs (_: file: final.callPackage file {});
  };
}
