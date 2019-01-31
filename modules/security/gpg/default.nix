{ pkgs, ... }:

let
  pinentry-rofi = pkgs.callPackage ./pinentry-rofi.nix { };
in

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
    extraConfig = "pinentry-program ${pinentry-rofi}";
  };
}
