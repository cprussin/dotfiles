{
  lib,
  statix,
  deadnix,
  alejandra,
  colmena,
  vulnix,
  callPackage,
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

  deploy = "${colmena}/bin/colmena apply";
  check-vulnerabilities = "${vulnix}/bin/vulnix --system";
  iot = callPackage ./iot.nix {};
  systems-test = callPackage ./systems-test.nix {};
}
