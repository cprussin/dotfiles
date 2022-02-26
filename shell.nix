let
  sources = import ./sources.nix;
in
  {
    nixpkgs ? sources.nixpkgs,
    niv ? sources.niv,
    nixops ? sources.nixops,
    alejandra ? sources.alejandra,
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

    nixops-overlay = _: _: {
      nixops = (import nixops).default;
    };

    alejandra-overlay = self: _: {
      alejandra = (import alejandra)."${self.system}";
    };

    password-utils-overlay = self: _: {
      passwordUtils = (self.callPackage ./lib/passwords.nix {}).passwordUtils;
    };

    pkgs = import nixpkgs {
      overlays = [
        niv-overlay
        nixops-overlay
        alejandra-overlay
        password-utils-overlay
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
      ${pkgs.nixops}/bin/nixops deploy "$@"
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

      rm -f iot-build/result.yaml*
      nix-build --out-link "iot-build/result.json" --attr "$2" ./config/iot >/dev/null
      ${replace-password-commands}/bin/replace-password-commands iot-build/result.json > iot-build/result.yaml
      ${pkgs.esphome}/bin/esphome iot-build/result.yaml** $1
    '';
  in
    pkgs.mkShell {
      buildInputs = [
        pkgs.git
        pkgs.niv
        pkgs.nixops
        pkgs.passwordUtils
        lint
        format
        deploy
        collect-garbage
        iot
      ];

      shellHook = "export NIX_PATH=\"nixpkgs=${nixpkgs}\"";
    }
