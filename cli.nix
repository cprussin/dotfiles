{
  lib,
  bash,
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
    dead-code = "${deadnix}/bin/deadnix --fail .";
    format = "${alejandra}/bin/alejandra --check .";

    # The one shell script on this fleet that is not a few lines inside a unit:
    # it runs unattended for hours and fills 97G, so its decisions are checked
    # rather than read.  statix and deadnix see Nix and nothing else.
    crux-tree-bootstrap = "${bash}/bin/bash ${./config/machines/crux/test-bootstrap-chromium-tree.sh} ${./config/machines/crux/bootstrap-chromium-tree.sh}";
  };

  fix = {
    lint = "${statix}/bin/statix fix .";
    dead-code = "${deadnix}/bin/deadnix -e .";
    format = "${alejandra}/bin/alejandra .";
  };

  deploy = "${colmena}/bin/colmena apply";
  check-vulnerabilities = "${vulnix}/bin/vulnix --system";
  iot = callPackage ./iot.nix {};
  systems-test = callPackage ./systems-test.nix {};
  upload-keys = "${colmena}/bin/colmena upload-keys --on crux";
  send-gpg-keys = writeShellScript "upload-gpg-keys" ''
    gpg --keyserver keyserver.ubuntu.com --send-key 0x426ABF93ACE024D0
    gpg --keyserver keys.openpgp.org --send-key 0x426ABF93ACE024D0
    gpg --keyserver pgp.mit.edu --send-key 0x426ABF93ACE024D0
  '';
}
