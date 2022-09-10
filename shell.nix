let
  sources = import ./sources.nix;
in
  {
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

    alejandra-overlay = self: _: {
      alejandra = (import alejandra)."${self.system}";
    };

    colmena-overlay = _: _: {
      colmena = import colmena;
    };

    password-utils-overlay = self: _: {
      passwordUtils = (self.callPackage ./lib/passwords.nix {}).passwordUtils;
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

    lint = pkgs.writeShellScriptBin "lint" ''
      ${pkgs.nix-linter}/bin/nix-linter ${files} "$@"
    '';

    format = pkgs.writeShellScriptBin "format" ''
      ${pkgs.alejandra}/bin/alejandra ${files} "$@"
    '';

    deploy = pkgs.writeShellScriptBin "deploy" ''
      ${pkgs.colmena}/bin/colmena apply "$@"
    '';

    collect-garbage = pkgs.writeShellScriptBin "collect-garbage" ''
      sudo ${pkgs.nix}/bin/nix-collect-garbage -d
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
  in
    pkgs.mkShell {
      buildInputs = [
        pkgs.git
        pkgs.niv
        pkgs.passwordUtils
        pkgs.colmena
        lint
        format
        deploy
        collect-garbage
        iot
      ];

      shellHook = "export NIX_PATH=\"nixpkgs=${nixpkgs}\"";
    }
