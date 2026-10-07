{
  description = "PIX - Peromage's nIX configuration";

  # When using this flake as an input in a downstream flake and overriding certain
  # inputs of this flake, for example, to use a different version of nixpkgs,
  # simply make it follow the altered version from the downstream flake. Other
  # inputs that follows it will be updated automatically, like `home-manager` (
  # follows persists)
  inputs = {
    # Linux
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    lanzaboote.url = "github:nix-community/lanzaboote/master";

    # Darwin
    # Duplicated flakes made specifically for Darwin system are suffixed by
    # `__darwin`
    nixpkgs__darwin.url = "github:nixos/nixpkgs/nixpkgs-26.05-darwin";
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs__darwin";
    };
    home-manager__darwin = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs__darwin";
    };
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    /*
    Meta
    */
    pix = self;
    lib = nixpkgs.lib;
    meta = {
      maintainer = {
        name = "Fang Deng";
        email = "fang@elfang.com";
        github = "peromage";
        githubId = 10389606;
      };
      license = lib.licenses.gpl3Plus;

      # May not require change
      # See: https://search.nixos.org/options?channel=unstable&show=system.stateVersion&query=stateVersion
      stateVersion = "25.11";
      # Different from NixOS stateVersion
      # See: https://nix-darwin.github.io/nix-darwin/manual/#opt-system.stateVersion
      darwinStateVersion = 6;
    };

    /*
    Lib with additional functions
    */
    libpix = (import ./lib (inputs // {inherit pix;})).extend (final: prev: {
      overlays = lib.attrValues pix.overlays;
    });
  in {
    /*
    Pix
    */
    inherit meta;
    lib = libpix;

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

    homeModules = {
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
    packages = libpix.forEachSupportedSystems (system:
      import ./packages {
        inherit pix system;
      });

    /*
    Development Shells

    Related commands:
      nix develop .#SHELL_NAME
    */
    devShells = libpix.forEachSupportedSystems (system:
      import ./devshells {
        inherit pix system;
      });

    /*
    Code Formatter

    Related commands:
      nix fmt

    Alternatively, `nixpkgs-fmt'
    */
    formatter = libpix.forEachSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

    /*
    Overlays

    Imported by other flakes
    */
    overlays = import ./overlays {inherit pix;};

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
      Framework = libpix.makeNixOS ./config/nixos-Framework-13;
      NUC = libpix.makeNixOS ./config/nixos-NUC-Server;
    };

    /*
    Darwin Configurations

    Related commands:
      darwin-rebuild switch --flake .#HOST_NAME
    */
    darwinConfigurations = {
      Macbook = libpix.makeDarwin ./config/darwin-Macbook-13;
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
      fang = libpix.makeHome "x86_64-linux" ./config/presets/user-fang/home-manager;
    };
  };
}
