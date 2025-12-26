{ pix }:

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
    pixPkgs = pix.packages.${final.stdenv.hostPlatform.system};
  };

  callPackageHelpers = final: prev: {
    callPackage = prev.newScope { inherit pix; };

    callPackageAttrs = autoArgs: let
      callPackage = prev.newScope ({ inherit pix; } // autoArgs);
    in pix.inputs.nixpkgs.lib.mapAttrs
      (_: file: callPackage file {});
  };
}
