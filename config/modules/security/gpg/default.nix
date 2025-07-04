{
  pkgs,
  lib,
  config,
  ...
}: {
  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.gnupg];

    programs.gpg = {
      enable = true;
      mutableKeys = false;
      mutableTrust = false;
      publicKeys = [
        {
          source = config.flake-inputs.gpg-key;
          trust = "ultimate";
        }
      ];
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      sshKeys = ["B8B6B7089A55037230D37A2D303706E909665E80"];
      pinentry.package = pkgs.pinentry-qt;
    };
  };
}
