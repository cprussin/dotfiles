{
  sources ? import ./sources.nix,
  nixpkgs ? sources.nixpkgs,
  niv ? sources.niv,
  colmena ? sources.colmena,
  mkCli ? sources.mkCli,
}: let
  niv-overlay = self: _: {
    niv = self.symlinkJoin {
      name = "niv";
      paths = [niv];
      buildInputs = [self.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/niv \
          --add-flags "--sources-file ${toString ./sources.json}"
      '';
    };
  };

  colmena-overlay = _: _: {
    colmena = import colmena;
  };

  password-utils-overlay = self: _: {
    inherit (self.callPackage ./lib/passwords.nix {}) passwordUtils;
  };

  mkCli-overlay = import "${mkCli}/overlay.nix";

  pkgs = import nixpkgs {
    overlays = [
      niv-overlay
      colmena-overlay
      password-utils-overlay
      mkCli-overlay
    ];
    config = {};
  };

  cli = pkgs.lib.mkCli "cli" {
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

    iot = pkgs.writeShellScript "iot" ''
      set -e

      cmd="$1"
      shift

      rm -f iot-build/*.{yaml,json}
      for target in "$@"; do
        nix-build --out-link "iot-build/''${target}.json" --attr "$target" ./config/iot >/dev/null
        ${pkgs.python3}/bin/python ./replace-password-commands.py "iot-build/''${target}.json" > "iot-build/''${target}.yaml"
      done

      ${pkgs.esphome}/bin/esphome $cmd iot-build/*.yaml
    '';
  };
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.git
      pkgs.niv
      pkgs.passwordUtils
      pkgs.colmena
      cli
    ];

    shellHook = "export NIX_PATH=\"nixpkgs=${nixpkgs}\"";
  }
