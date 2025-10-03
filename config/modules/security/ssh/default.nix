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
    userKnownHostsFile = "${./known_hosts}";
    controlMaster = "auto";
    controlPersist = "1m";
    matchBlocks = {
      rsync = {
        user = "zh2593";
        hostname = "zh2593.rsync.net";
      };
      "*".identityFile = config.deployment.keys.sshClientKey.path;
    };
  };
}
