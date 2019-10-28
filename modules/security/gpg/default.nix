{ config, ... }:

{
  nixpkgs.overlays = [
    (
      self: super: {
        gnupg = self.callPackage ./gnupg.nix {
          inherit config;
          gnupg = super.gnupg;
        };
      }
    )
  ];

  home-manager.users.${config.primaryUserName}.services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };
}
