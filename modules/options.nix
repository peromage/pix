{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    types
    filterAttrs
    mapAttrs
    attrNames
    attrValues
    all
    isPath;

  systemCfg = config.pix.system;
  userCfg = config.pix.users;

  userSettings = { name, config, ... }: {
    options = {
      nologin = mkEnableOption "login for user ${name}";

      password = mkOption {
        type = with types; nullOr (either str path);
        default = null;
        description = ''
          This option serves three purposes.
            - If `pix.system.immutableUsers' option is disabled, the value will
               be used as `initialPassword'.
            - If `pix.system.immutableUsers' option is enabled and the value is
              a path, it will be used as `hashedPasswordFile'.
            - If `pix.system.immutableUsers' option is enabled and the value is
              a string, it will be used as `hashedPassword'.

          Hashed password can be generated by `mkpassword'.
          The value will be set automatically when `nologin' option is true, which
          is used as the `hashedPassword' to prevent trivial login.  However, it
          can be overridden outside of VC (instead of the default value).
        '';
      };

      ## The following settings has no effect to root user
      ## See: `mkUser' and `mkRootUser'
      enableNixManagement = mkEnableOption "Nix trusted user";

      description = mkOption {
        type = types.str;
        default = "";
        description = "User description.";
      };

      id = mkOption {
        type = with types; nullOr int;
        default = null;
        description = "User's UID and GID.";
      };

      groups = mkOption {
        type = with types; listOf str;
        default = [];
        description = "Groups that user belongs to.";
      };
    };

    ## Can be hardened by overriding the password outside of VC (flake template).
    config = mkIf config.nologin {
      password = "**DISABLED!**";
    };
  };

  isRootUser = name: "root" == name;

  filterNormalUser = filterAttrs (name: _: ! isRootUser name);

  ## NOTE: When nologin is enabled use an arbitrary password as the hashed password
  ## to prevent from login with a normal password.
  ## When immutable is enabled, ensure the hashed password is generated by
  ## `mkpassword' CLI.
  definePassword = nologin: immutable: password:
    if isPath password then
      { hashedPasswordFile = password; }
    else if nologin || immutable then
      { hashedPassword = password; }
    else
      { initialPassword = password; };

  mkUser = name: config: immutable: {
    name = name;
    uid = config.id;
    group = name;
    extraGroups = config.groups;
    description = config.description;
    isNormalUser = true;
    isSystemUser = false;
    home = "/home/${name}";
    homeMode = "700";
    createHome = true;
  } // (definePassword config.nologin immutable config.password);

  mkRootUser = config: immutable: definePassword config.nologin immutable config.password;

in {
  /* System Options */
  options.pix.system = {
    hostName = mkOption {
      type = with types; nullOr str;
      default = "PIX";
      description = "Host name for this machine.";
    };

    immutableUsers = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Immutable user management.
        Note that when this is enabled the `hashedPassword' must be specified
        for each user declared within pix namespace.
      '';
    };
  };

  /* User Options */
  options.pix.users = mkOption {
    type = with types; attrsOf (submodule userSettings);
    default = {};
    description = ''
      User profile definitions.
        NOTE: root can be defined here as well but only a few options will be
        effective to it.
    '';
  };

  config = {
    networking.hostName = systemCfg.hostName;

    users.users = mapAttrs
      (name: config: if isRootUser name
                     then mkRootUser config systemCfg.immutableUsers
                     else mkUser name config systemCfg.immutableUsers)
      userCfg;

    users.groups = mapAttrs
      (_: config: { gid = config.id; })
      (filterNormalUser userCfg);

    nix.settings.trusted-users = attrNames (filterAttrs (_: v: v.enableNixManagement) (filterNormalUser userCfg));

    assertions = [
      {
        assertion = all (config: config.password != null) (attrValues userCfg);
        message = "All users must provide their passwords.";
      }
    ];
  };
}
