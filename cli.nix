{
  lib,
  statix,
  deadnix,
  alejandra,
  colmena,
  vulnix,
  callPackage,
  writeShellScript,
  ...
}:
lib.mkCli "cli" {
  _noAll = true;

  test = {
    lint = "${statix}/bin/statix check .";
    dead-code = "${deadnix}/bin/deadnix .";
    format = "${alejandra}/bin/alejandra --check .";
  };

  fix = {
    lint = "${statix}/bin/statix fix .";
    dead-code = "${deadnix}/bin/deadnix -e .";
    format = "${alejandra}/bin/alejandra .";
  };

  deploy = "${colmena}/bin/colmena --experimental-flake-eval apply";
  check-vulnerabilities = "${vulnix}/bin/vulnix --system";
  iot = callPackage ./iot.nix {};
  systems-test = callPackage ./systems-test.nix {};
  upload-keys = "${colmena}/bin/colmena --experimental-flake-eval upload-keys --on crux";
  send-gpg-keys = writeShellScript "upload-gpg-keys" ''
    gpg --keyserver keyserver.ubuntu.com --send-key 0x426ABF93ACE024D0
    gpg --keyserver keys.openpgp.org --send-key 0x426ABF93ACE024D0
    gpg --keyserver pgp.mit.edu --send-key 0x426ABF93ACE024D0
  '';
}
