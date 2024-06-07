{ pkgs, ... }: pkgs.lib.mkCli "cli" {
  _noAll = true;

  test = {
    lint = "${pkgs.statix}/bin/statix check .";
    dead-code = "${pkgs.deadnix}/bin/deadnix .";
    format = "${pkgs.alejandra}/bin/alejandra --check .";
  };

  fix = {
    lint = "${pkgs.statix}/bin/statix fix .";
    dead-code = "${pkgs.deadnix}/bin/deadnix -e .";
    format = "${pkgs.alejandra}/bin/alejandra .";
  };

  deploy = "${pkgs.colmena}/bin/colmena apply";
  build-iso = "${pkgs.nix}/bin/nix build -f ./isos --out-link ./iso-build";
  collect-garbage = "sudo ${pkgs.nix}/bin/nix-collect-garbage -d";
  check-vulnerabilities = "${pkgs.vulnix}/bin/vulnix --system";
  iot = pkgs.callPackage ./iot.nix {};
  systems-test = pkgs.callPackage ./systems-test.nix {};
}
