{ config, ... }:

#let
#  gpg-config = pkgs.writeText "gpg-config" ''
#    photo-viewer "${pkgs.launcher}/bin/open %i"
#    keyserver hkp://pgp.mit.edu
#  '';
#in

{
  nixpkgs.overlays = [
    (self: super: {
      gnupg = self.callPackage ./gnupg.nix {
        inherit config;
        gnupg = super.gnupg;
      };
    })
  ];

  home-manager.users.${config.primaryUserName} = { ... }: {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
}
