{ pkgs, lib, ... }:

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
  primary-user.home-manager = {
    home.file = {
      ".ssh/known_hosts".source = ./known_hosts;
      ".ssh/authorized_keys".source = ./authorized_keys;
      ".ssh/environment".text = "PATH=${ssh-path}";
    };

    programs.ssh = {
      enable = true;
      matchBlocks = [
        {
          host = "prussin.net *.bci-incorporated.com";
          user = "connor";
          port = 3580;
        }
        {
          host = "aur.archlinux.org";
          user = "aur";
        }
      ];
    };
  };
}
