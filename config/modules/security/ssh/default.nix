{
  pkgs,
  config,
  ...
}: let
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
    settings = {
      rsync = {
        User = "zh2593";
        HostName = "zh2593.rsync.net";
      };
      "*" = {
        ForwardAgent = false;
        AddKeysToAgent = "no";
        IdentityFile = config.deployment.keys.sshClientKey.path;
        ControlMaster = "auto";
        ControlPersist = "1m";
        UserKnownHostsFile = "${./known_hosts}";
      };
    };
  };
}
