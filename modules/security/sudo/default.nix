{ lib, pkgs, config, ... }:

let
  nopasswd = cmd: {
    command = cmd;
    options = [ "NOPASSWD" ];
  };
in

{
  options.sudoCmds = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
    description = ''
      The list of commands to allow the primary user to use via sudo without
      password.
    '';
  };

  config = {
    nixpkgs.overlays = [
      (self: super: {
        sudo = super.sudo.override {
          withInsults = true;
        };
      })
    ];

    security.sudo = {
      extraConfig = "Defaults insults";
      extraRules = lib.mkAfter [{
        users = [ config.primaryUserName ];
        commands = (map nopasswd config.sudoCmds);
      }];
    };

    users.users.${config.primaryUserName}.extraGroups = [ "wheel" ];
  };
}
