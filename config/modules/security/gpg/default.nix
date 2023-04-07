{
  pkgs,
  lib,
  ...
}: let
  sources = import ../../../../sources.nix;
in {
  services.pcscd.enable = true;

  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.gnupg];

    programs.gpg = {
      enable = true;
      mutableKeys = false;
      mutableTrust = false;
      publicKeys = [
        {
          source = sources.gpg-key;
          trust = "ultimate";
        }
      ];
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      sshKeys = ["B8B6B7089A55037230D37A2D303706E909665E80"];
      pinentryFlavor = "qt";
    };
  };
}
