{
  pkgs,
  lib,
  ...
}: let
  sources = import ../../../../sources.nix;
in {
  services.pcscd.enable = true;

  # These mobules break my NFC reader...
  boot.blacklistedKernelModules = ["pn533" "pn533_usb"];

  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.gnupg];

    programs.gpg = {
      enable = true;
      mutableKeys = false;
      mutableTrust = false;
      scdaemonSettings = {
        disable-ccid = true;
        pcsc-driver = "${lib.getLib pkgs.pcsclite}/lib/libpcsclite${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
      };
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
