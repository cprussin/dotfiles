{ ... }:

{
  home.file.".gnupg/gpg.conf".text = ''
    armor
    photo-viewer "open %i"
    keyserver hkp://pgp.mit.edu
  '';
}
