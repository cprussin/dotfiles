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
      pinentry.package = pkgs.pinentry-qt;
    };
  };
}
