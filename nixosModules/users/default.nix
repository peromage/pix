{ config, lib, pix, ... }:

let
  libpix = pix.lib;
  cfg = config.pix.users;
  isRootUser = name: name == "root";

in with lib; {
  imports = with libpix; listDir (notPred isDefaultNix) ./.;

  /* Interface */
  options.pix.users = {
    immutable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Immutable user management.
        Note that when this is enabled the `hashedPassword' must be specified
        for each user declared within pix namespace.
      '';
    };

    /* Normal user profile options */
    profiles = let
      userProfileOptions = { name, config, ... }: with lib; {
        options = {
          enable = mkEnableOption "user ${name}";

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

          password = mkOption {
            type = with types; nullOr (either str path);
            default = null;
            description = ''
              This option serves three purposes.
          - If `immutable' option is disabled, the value will be used as
            `initialPassword'.
          - If `immutable' option is enabled and the value is a path, it will be
            used as `hashedPasswordFile'.
          - If `immutable' option is enabled and the value is a string, it will
            be used as `hashedPassword'.

          Hashed password can be generated by `mkpassword'.
            '';
          };
        };

        ## Disable root login by setting an invalid hashed password (if disabled).
        ## May be hardened by overriding the password outside of VC (flake template).
        config = mkIf (isRootUser name && ! config.enable) {
          password = "**DISABLED!**";
        };
      };

    in mkOption {
      type = with types; attrsOf (submodule userProfileOptions);
      default = {};
      description = ''
        User profile definitions.
        NOTE: root can be defined here as well but only a few options will be
        effective to it.
      '';
    };
  };

  /* Implementation */
  config = let
    definePassword = immutable: password:
      if immutable then
        if isPath password then
          { hashedPasswordFile = password; }
        else
          { hashedPassword = password; }
      else
        { initialPassword = password; };

    enabledNormalUsers = filterAttrs
      (name: config: ! isRootUser name && config.enable)
      cfg.profiles;

    ## NOTE: To get root disabling effective within this config, at least one
    ## normal user must be enabled.
  in mkIf (libpix.anyEnable cfg.profiles) {
    ## Immutable user option
    users.mutableUsers = ! cfg.immutable;

    users.users = mkMerge [
      ## Normal users
      (mapAttrs
        (name: config: {
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
        } // (definePassword cfg.immutable config.password))
        enabledNormalUsers)

      ## Root user
      {
        root = let
          rootCfg = cfg.profiles.root;
        in if rootCfg.enable
           then definePassword cfg.immutable rootCfg.password
           else { hashedPassword = rootCfg.password; };
      }
    ];

    ## User groups
    users.groups = mapAttrs'
      (name: config: nameValuePair name { gid = config.id; })
      enabledNormalUsers;

    ## Nix trusted users
    nix.settings.trusted-users = mapAttrsToList
      (name: config: name)
      (filterAttrs (name: config: config.enableNixManagement) enabledNormalUsers);

    ## Assertions
    assertions = [
      {
        assertion = all (config: config.password != null) (attrValues enabledNormalUsers);
        message = "Password must be provided.";
      }

      {
        assertion = let
          rootCfg = cfg.profiles.root;
        in rootCfg.enable -> rootCfg.password != null;
        message = "Root password must be provided when it is enabled.";
      }
    ];
  };
}
