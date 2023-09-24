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

      jq="${pkgs.jq}/bin/jq"
      cmd="$1"
      shift

      rm -f iot-build/*.{yaml,json}
      for target in "$@"; do
        nix-build --out-link "iot-build/''${target}.json" --attr "$target" ./config/iot >/dev/null

        # Replace password commands with password values by:
        # 1) Extract the list of password commands using jq
        # 2) Send the list through bash and jq -R to actually execute the
        #    commands and format the output into json strings
        # 3) Build a subshell that outputs, first, the raw json input file, and
        #    second the list of json-stringified passwords from step 2
        # 4) Pipe the output of the subshell in step 3 into another jq command
        #    that recursively looks for password commands.  When it finds one,
        #    it replaces it with the next input.
        # This works because the order of the list generated in step 2 should
        # always match the order that jq's recursive walk finds key commands.
        # If for some reason that assumption ever fails, we'd need to change 2
        # to output something that actually keys the passwords by command to
        # reconcile them later, or switch back to python or something, but I'd
        # prefer to stay in jq for something this simple.
        (
            cat "./iot-build/''${target}.json";
            $jq -r '.. | .keyCommand? | select(. != null) | map("\"\(.)\"") | join(" ")' <"iot-build/''${target}.json" | bash | $jq -R
        ) | $jq -c 'walk(if type == "object" and .keyCommand? then input else . end)' > "iot-build/''${target}.yaml"
      done

      ${pkgs.esphome}/bin/esphome $cmd iot-build/*.yaml
    '';
  };
in
  pkgs.mkShell {
    NIX_PATH = "nixpkgs=${nixpkgs}";

    buildInputs = [
      pkgs.git
      pkgs.niv
      pkgs.passwordUtils
      pkgs.colmena
      cli
    ];
  }
