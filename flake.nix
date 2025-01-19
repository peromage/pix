{
  description = "PIX - Peromage's nIX configuration";

  inputs = {
    /* Common flakes */
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    lanzaboote = { url = "github:nix-community/lanzaboote/master"; inputs.nixpkgs.follows = "nixpkgs"; };
    home-manager = { url = "github:nix-community/home-manager/master"; inputs.nixpkgs.follows = "nixpkgs"; };

    /* Mac specialized */
    nix-darwin = { url = "github:LnL7/nix-darwin/master"; inputs.nixpkgs.follows = "nixpkgs"; };

    /* Some useful flakes */
    # nix-colors = { url = "github:misterio77/nix-colors/main"; inputs.nixpkgs.follows = "nixpkgs"; };
    # nix-alien = { url = "github:thiagokokada/nix-alien/master"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs = { self, nixpkgs, home-manager, nix-darwin, ... }:
    let
      pix = self;

      /* Meta */
      license = nixpkgs.lib.licenses.gpl3Plus;
      maintainer = {
        name = "Fang Deng";
        email = "fang@elfang.com";
        github = "peromage";
        githubId = 10389606;
      };

      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      /* Lib with additional functions */
      lib = (import ./lib { inherit nixpkgs; }) // (with lib; {
        forEachSupportedSystems = with nixpkgs.lib; genAttrs supportedSystems;

        mkPkgs = system: import nixpkgs {
          inherit system;
          overlays = with self.outputs.overlays; [ unrestrictedPkgs pixPkgs ];
        };

        callPackageOn = fn: system: (mkPkgs system).newScope { inherit pix; } fn {};
        importPackage = fn: import fn { inherit pix; };

        /* Note that the `system' attribute is not explicitly set (default to null)
           to allow modules to set it themselves.  This allows a hermetic configuration
           that doesn't depend on the system architecture when it is imported.
           See: https://github.com/NixOS/nixpkgs/pull/177012
        */
        mkNixOS = fn: mkConfiguration nixpkgs.lib.nixosSystem (_: {
          specialArgs = { inherit pix; };
          modules = [
            fn
            { nixpkgs.overlays = with self.outputs.overlays; [ unrestrictedPkgs pixPkgs ]; }
          ];
        });

        mkDarwin = fn: mkConfiguration nix-darwin.lib.darwinSystem (_: {
          specialArgs = { inherit pix; };
          modules = [ fn ];
        });

        mkHome = system: fn: mkConfiguration home-manager.lib.homeManagerConfiguration (_: {
          pkgs = mkPkgs system;
          extraSpecialArgs = { inherit pix; };
          modules = [ fn ];
        });
      });

    in with lib; {
      /* Pix */
      inherit license maintainer supportedSystems pix lib;

      /* Expose modules

         NOTE: Both `nixos' and `homeManager' module require an additional `pix'
         argument (I.E. this flake).  Don't forget to pass it in the `specialArgs'
         when importing them.  This is to bypass the infinite recursion problem
         where these modules are written in self-contained way.
      */
      nixosModules = {
        default = self.outputs.nixosModules.nixos;
        nixos = import ./modules { inherit pix; };
        dotfiles = import ./dotfiles { inherit pix; };
      };

      /* Packages

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
      packages = forEachSupportedSystems (callPackageOn ./packages);

      /* Development Shells

         Related commands:
           nix develop .#SHELL_NAME
      */
      devShells = forSupportedSystems (callPackageOn ./devshells);

      /* Code Formatter

         Related commands:
           nix fmt

         Alternatively, `nixpkgs-fmt'
      */
      formatter = forSupportedSystems (system: (mkPkgs system).alejandra);

      /* Overlays

         Imported by other flakes
      */
      overlays = importPackage ./overlays;

      /* Templates

         Related commands:
           nix flake init -t /path/to/this_config#TEMPLATE_NAME
      */
      templates = importPackage ./templates;

      /* NixOS Configurations

         Related commands:
           nixos-rebuild build|boot|switch|test --flake .#HOST_NAME
      */
      nixosConfigurations = {
        Framework = mkNixOS ./configurations/nixos-Framework-13;
        NUC = mkNixOS ./configurations/nixos-NUC-Server;
      };

      /* Darwin Configurations

         Related commands:
           darwin-rebuild switch --flake .#HOST_NAME
      */
      darwinConfigurations = {
        Macbook = mkDarwin ./configurations/darwin-Macbook-13;
      };

      /* HomeManager Configurations

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
        fang = mkHome "x86_64-linux" ./configurations/presets/user-fang/home;
      };
    };
}
