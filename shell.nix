{
  sources ? import ./sources.nix,
  nixpkgs ? sources.nixpkgs,
  nixpkgs-unstable ? sources.nixpkgs-unstable,
  niv ? sources.niv,
  alejandra ? sources.alejandra,
  colmena ? sources.colmena,
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

  alejandra-overlay = _: _: {
    alejandra = import alejandra {};
  };

  colmena-overlay = _: _: {
    colmena = import colmena;
  };

  password-utils-overlay = self: _: {
    inherit (self.callPackage ./lib/passwords.nix {}) passwordUtils;
  };

  pkgs-unstable = import nixpkgs-unstable {
    overlays = [];
    config = {};
  };

  unstable-esphome-overlay = _: _: {
    inherit (pkgs-unstable) esphome;
  };

  pkgs = import nixpkgs {
    overlays = [
      niv-overlay
      alejandra-overlay
      colmena-overlay
      password-utils-overlay
      unstable-esphome-overlay
    ];
    config = {};
  };

  files = "$(find . -name '*.nix')";

  check = pkgs.writeShellScriptBin "check" ''
    ${pkgs.nix-linter}/bin/nix-linter ${files} && \
    ${pkgs.statix}/bin/statix check . && \
    ${pkgs.deadnix}/bin/deadnix . && \
    ${pkgs.alejandra}/bin/alejandra --check ${files}
  '';

  fix = pkgs.writeShellScriptBin "fix" ''
    ${pkgs.alejandra}/bin/alejandra ${files} && \
    ${pkgs.statix}/bin/statix fix . && \
    ${pkgs.deadnix}/bin/deadnix -e .
  '';

  deploy = pkgs.writeShellScriptBin "deploy" ''
    ${pkgs.colmena}/bin/colmena apply "$@"
  '';

  collect-garbage = pkgs.writeShellScriptBin "collect-garbage" ''
    sudo ${pkgs.nix}/bin/nix-collect-garbage -d
  '';

  check-vulnerabilities = pkgs.writeShellScriptBin "check-vulnerabilities" ''
    ${pkgs.vulnix}/bin/vulnix --system
  '';

  replace-password-commands = pkgs.writers.writePython3Bin "replace-password-commands" {} ''
    import json
    import subprocess
    import sys


    def replace_password_commands(o):
        if 'passwordCommand' in o:
            out = subprocess.check_output(o['passwordCommand'])
            o['password'] = out.decode('UTF-8').strip()
            del o['passwordCommand']
        for k, v in o.items():
            if isinstance(v, dict):
                o[k] = replace_password_commands(o[k])
        return o


    def main():
        obj = json.load(open(sys.argv[1]))
        print(json.dumps(replace_password_commands(obj)))


    if __name__ == '__main__':
        main()
  '';

  iot = pkgs.writeShellScriptBin "iot" ''
    set -e

    cmd="$1"
    shift

    rm -f iot-build/*.{yaml,json}
    for target in "$@"; do
      nix-build --out-link "iot-build/''${target}.json" --attr "$target" ./config/iot >/dev/null
      ${replace-password-commands}/bin/replace-password-commands "iot-build/''${target}.json" > "iot-build/''${target}.yaml"
    done

    ${pkgs.esphome}/bin/esphome $cmd iot-build/*.yaml
  '';

  build-iso = pkgs.writeShellScriptBin "build-iso" "nix build -f ./isos $1";
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.git
      pkgs.niv
      pkgs.passwordUtils
      pkgs.colmena
      check
      fix
      deploy
      check-vulnerabilities
      collect-garbage
      iot
      build-iso
    ];

    shellHook = "export NIX_PATH=\"nixpkgs=${nixpkgs}\"";
  }
