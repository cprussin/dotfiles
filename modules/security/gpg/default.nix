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
        gnupg = super.gnupg;
        secure = config.secure;
      };
    })
  ];

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };
}
