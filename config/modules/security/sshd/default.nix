{ pkgs, lib, config, ... }:

let
  ssh-path = lib.makeBinPath [
    pkgs.coreutils
    pkgs.gnused
    pkgs.gnugrep
    pkgs.git
    pkgs.openssh
  ];
in

{
  options.enableSshdAtBoot = lib.mkEnableOption "SSH daemon auto-start at boot";

  config = {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      extraConfig = "PermitUserEnvironment yes";
    };

    systemd.services.sshd.wantedBy = lib.mkIf config.enableSshdAtBoot (lib.mkForce []);

    primary-user = {
      openssh.authorizedKeys.keys = [
        (builtins.extraBuiltins.publicSshKey pkgs config)
      ];

      home-manager.home.file.".ssh/environment".text = "PATH=${ssh-path}";
    };
  };
}
