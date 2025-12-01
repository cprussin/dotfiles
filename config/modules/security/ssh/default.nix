{ pkgs, config, ... }: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
in {
 deployment.keys.sshClientKey = {
   keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssh/cprussin";
   user = config.primary-user.name;
   group = "users";
   destDir = "/secrets";
 };

  primary-user.home-manager.programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      rsync = {
        user = "zh2593";
        hostname = "zh2593.rsync.net";
      };
      "*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        identityFile = config.deployment.keys.sshClientKey.path;
        controlMaster = "auto";
        controlPersist = "1m";
        userKnownHostsFile = "${./known_hosts}";
      };
    };
  };
}
