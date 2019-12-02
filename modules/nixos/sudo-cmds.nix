{ config, lib, ... }:

let
  cfg = config.sudo-cmds;

  nopasswd = command: {
    inherit command;
    options = [ "NOPASSWD" ];
  };
in

{
  options.sudo-cmds = lib.mkOption {
    type = lib.types.attrsOf (lib.types.listOf lib.types.str);
    default = {};
    description = ''
      An attrset mapping usernames to lists of sudo commands to allow those
      users to run without passwords.
    '';
  };

  config.security.sudo.extraRules = lib.mkAfter (
    lib.mapAttrsToList (
      username: commands: {
        users = [ username ];
        commands = map nopasswd commands;
      }
    ) cfg
  );
}
