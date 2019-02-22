{ pkgs, ... }:

{
  home.file = {
    ".gnupg/gpg.conf".text = ''
      armor
      photo-viewer "open %i"
      keyserver hkp://pgp.mit.edu
    '';
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    extraConfig = "pinentry-program ${pkgs.launcher}/bin/pinentry";
  };
}
