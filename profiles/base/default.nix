{ config, ... }:

let
  nixosVersion = import ../../nixos-version.nix;
  home-manager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/release-${nixosVersion}.tar.gz";
in

{
  imports = [
    ../../modules/devices/tmp

    ../../modules/security/primary-user
    ../../modules/security/process-information-hiding
    ../../modules/security/sudo

    ../../modules/ui/dvp/system
    ../../modules/ui/greeting

    "${home-manager}/nixos"
  ];

  home-manager.users.${config.primaryUserName} = { lib, pkgs, config, ... }: {
    imports = [
      ../../modules/security/umask

      ../../modules/ui/bash
      ../../modules/ui/dvp/user
      ../../modules/ui/readline
      ../../modules/ui/zsh
    ];

    home.packages = lib.mkForce [

      # FIXME: This is done under the hood in home-manager to set
      # sessionVariables.  We do still want this in the environment, even if we
      # want to clear out the other automatically added stuff.  Ideally there
      # should be a simpler way to clear the environment except this file.
      (pkgs.writeTextFile {
        name = "hm-session-vars.sh";
        destination = "/etc/profile.d/hm-session-vars.sh";
        text = ''
          # Only source this once.
          if [ -n "$__HM_SESS_VARS_SOURCED" ]; then return; fi
          export __HM_SESS_VARS_SOURCED=1
          ${config.lib.shell.exportAll config.home.sessionVariables}
        '';
      })

    ];
  };
}
