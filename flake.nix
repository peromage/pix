{
  description = "PIX - Peromage's nIX configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
    home-manager = { url = "github:nix-community/home-manager/release-25.11"; inputs.nixpkgs.follows = "nixpkgs"; };

    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    lanzaboote = { url = "github:nix-community/lanzaboote/master"; inputs.nixpkgs.follows = "nixpkgs"; };
    # nix-colors = { url = "github:misterio77/nix-colors/main"; inputs.nixpkgs.follows = "nixpkgs"; };
    # nix-alien = { url = "github:thiagokokada/nix-alien/master"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs = { self, nixpkgs, home-manager, nix-darwin, ... }:
    let
      /*
         Meta
      */
      stateVersion = "25.11";
      pix = self;
      license = nixpkgs.lib.licenses.gpl3Plus;
      maintainer = {
        name = "Fang Deng";
        email = "fang@elfang.com";
        github = "peromage";
        githubId = 10389606;
      };

      /*
         Lib with additional functions
      */
      lib = (import ./lib { inherit nixpkgs; }).extend (final: prev: {
        pkgsOverlays = with self.outputs.overlays; [
          unrestrictedPkgs
          pixPkgs
          callPackageHelpers
        ];

        supportedSystems = [
          "x86_64-linux"
          "x86_64-darwin"
          "aarch64-linux"
          "aarch64-darwin"
        ];

        forEachSupportedSystems = nixpkgs.lib.genAttrs final.supportedSystems;

        makePkgs = system: import nixpkgs {
          inherit system;
          overlays = final.pkgsOverlays;
        };

        /*
           Note that the `system' attribute is not explicitly set (default to null)
           to allow modules to set it themselves.  This allows a hermetic configuration
           that doesn't depend on the system architecture when it is imported.
           See: https://github.com/NixOS/nixpkgs/pull/177012
        */
        makeNixOS = fn: final.makeConfiguration nixpkgs.lib.nixosSystem (_: {
          specialArgs = { inherit pix; };
          modules = [
            self.outputs.nixosModules.default
            {
              nixpkgs.overlays = final.pkgsOverlays;
              system.stateVersion = stateVersion;
            }
            fn
          ];
        });

        makeDarwin = fn: final.makeConfiguration nix-darwin.lib.darwinSystem (_: {
          specialArgs = { inherit pix; };
          modules = [
            {
              # Different from NixOS stateVersion
              # See: https://nix-darwin.github.io/nix-darwin/manual/#opt-system.stateVersion
              system.stateVersion = 4;
            }
            fn
          ];
        });

        makeHome = system: fn: final.makeConfiguration home-manager.lib.homeManagerConfiguration (_: {
          pkgs = final.makePkgs system;
          extraSpecialArgs = { inherit pix; };
          modules = [
            self.outputs.homemanagerModules.default
            {
              home.stateVersion = stateVersion;
            }
            fn
          ];
        });
      });

    in with lib; {
      /*
         Pix
      */
      inherit license maintainer pix lib;

      /*
         Expose modules

         NOTE: Both `nixos' and `homeManager' module require an additional `pix'
         argument (I.E. this flake).  Don't forget to pass it in the `specialArgs'
         when importing them.  This is to bypass the infinite recursion problem
         where these modules are written in self-contained way.
      */
      nixosModules = {
        default = import ./modules;
      };

      homemanagerModules = {
        default = import ./dotfiles;
      };

      /*
         Packages

         Related commands:
           nix build .#PACKAGE_NAME
           nix shell
           home-manager build|switch --flake .#NAME

         Notice that there is a minor difference between `packages' and `legacyPackages'.

         From: https://github.com/NixOS/nixpkgs/blob/b2e41a5bd20d4114f27fe8d96e84db06b841d035/flake.nix#L47

         The "legacy" in `legacyPackages` doesn't imply that the packages exposed
         through this attribute are "legacy" packages. Instead, `legacyPackages`
         is used here as a substitute attribute name for `packages`. The problem
         with `packages` is that it makes operations like `nix flake show
         nixpkgs` unusably slow due to the sheer number of packages the Nix CLI
         needs to evaluate. But when the Nix CLI sees a `legacyPackages`
         attribute it displays `omitted` instead of evaluating all packages,
         which keeps `nix flake show` on Nixpkgs reasonably fast, though less
         information rich.
      */
      packages = forEachSupportedSystems (system: import ./packages { inherit pix; pkgs = makePkgs system; });

      /*
         Development Shells

         Related commands:
           nix develop .#SHELL_NAME
      */
      devShells = forEachSupportedSystems (system: import ./devshells { inherit pix; pkgs = makePkgs system; });

      /*
         Code Formatter

         Related commands:
           nix fmt

         Alternatively, `nixpkgs-fmt'
      */
      formatter = forEachSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      /*
         Overlays

         Imported by other flakes
      */
      overlays = import ./overlays { inherit pix; };

      /*
         Templates

         Related commands:
           nix flake init -t /path/to/this_config#TEMPLATE_NAME
      */
      templates = import ./templates {};

      /*
         NixOS Configurations

         Related commands:
           nixos-rebuild build|boot|switch|test --flake .#HOST_NAME
      */
      nixosConfigurations = {
        Framework = makeNixOS ./config/nixos-Framework-13;
        NUC = makeNixOS ./config/nixos-NUC-Server;
      };

      /*
         Darwin Configurations

         Related commands:
           darwin-rebuild switch --flake .#HOST_NAME
      */
      darwinConfigurations = {
        Macbook = makeDarwin ./config/darwin-Macbook-13;
      };

      /*
         HomeManager Configurations

         Related commands:
           nix build .#homeConfigurations.SYSTEM.NAME.activationPackage

         NOTE: The Home Manager command:
           home-manager build|switch --flake .#NAME

         looks for `homeConfigurations.user' with pre-defined platform arch in
         user config.  This is not flexible.  Instead, this section is set to
         `homeConfigurations.arch.user' and mapped to
         `packages.arch.homeConfigurations.user' and the command will pick it
         from there automatically.
      */
      homeConfigurations = {
        fang = makeHome "x86_64-linux" ./config/presets/user-fang/home-manager;
      };
    };
}
