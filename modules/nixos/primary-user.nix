{ config, lib, ... }:

let
  cfg = config.primary-user;
in

{
  options.primary-user = {
    name = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "The name of the primary user account.";
    };

    home = lib.mkOption {
      type = lib.types.str;
      description = "The path to the primary user's home.";
    };

    shell = lib.mkOption {
      type = lib.types.str;
      description = "The path to the primary user's shell.";
    };

    extraGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = ''
        A list of additional groups that the primary user belongs to.
      '';
    };

    sudo-cmds = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = ''
        A of sudo commands to allow the primary user to run without entering a
        password.
      '';
    };

    uid = lib.mkOption {
      type = lib.types.int;
      description = "The user ID for the primary user account.";
      default = 1000;
    };
  };

  config = lib.mkIf (cfg.name != null) {
    primary-user = {
      home = lib.mkDefault "/home/${cfg.name}";
      shell = lib.mkDefault config.users.defaultUserShell;
      extraGroups = [ "wheel" ];
    };
    users.users.${cfg.name} = {
      isNormalUser = true;
      home = cfg.home;
      uid = cfg.uid;
      extraGroups = cfg.extraGroups;
      shell = cfg.shell;
    };
    #home-manager.users.${cfg.name} = cfg.home;
    sudo-cmds.${cfg.name} = cfg.sudo-cmds;
    nix.trustedUsers = [ "root" cfg.name ];
  };
}
