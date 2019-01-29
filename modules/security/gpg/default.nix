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
    ".gnupg/gpg-agent.conf".text = ''
      pinentry-program ${pinentry-rofi}
    '';
  };
}
