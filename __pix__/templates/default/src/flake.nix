{
  inputs = {
    /* Sample flake.

       This is a demostration of how to use pix configuration in a nondeterministic
       way.

       Do not build this directly on your setup as it will fail.

       Override the release version
       For example:

       nixpkgs-2305.url = "github:nixos/nixpkgs/nixos-23.05";
       rice.inputs.nixpkgs.follows = "nixpkgs-2305";
    */
    rice.url = "github:peromage/rice/master";
  };

  outputs = { self, rice, ... }:
    let
      lib = rice.inputs.nixpkgs.lib;

    in {
      nixosConfigurations = {
        Laptop = with lib; rice.nixosConfigurations.Framework.extraModules [
          {
            pix.hosts.hostName = mkForce "Foo";
            pix.users.immutable = true;
            ## NOTE: The hashed password can be generated by `mkpassword'.
            pix.users.profiles.fang.password = "mkpassword";
          }

          /* Other modules */
        ];
      };
    };
}
