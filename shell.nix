let
  sources = import ./sources.nix;
in

{ nixpkgs ? sources.nixpkgs
, niv ? sources.niv
, nixops ? sources.nixops
}:

  let
    niv-overlay = self: _: {
      niv = self.symlinkJoin {
        name = "niv";
        paths = [ niv ];
        buildInputs = [ self.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/niv \
            --add-flags "--sources-file ${toString ./sources.json}"
        '';
      };
    };

    nixops-built = (import nixops).default;

    nixops-wrapped = pkgs: pkgs.writeShellScriptBin "nixops" ''
      cmd=$1
      shift

      export NIX_PATH="nixpkgs=${nixpkgs}:nixpkgs-overlays=$(NIX_PATH=nixpkgs=${nixpkgs} nix-build --no-out-link)/overlays"

      exec ${nixops-built}/bin/nixops $cmd \
        --option plugin-files ${pkgs.nix-plugins}/lib/nix/plugins/libnix-extra-builtins.so \
        --option extra-builtins-file ${./extra-builtins.nix} \
        "$@"
    '';

    nixops-overlay = self: _: {
      nixops = self.symlinkJoin {
        name = "nixops";
        paths = [
          (nixops-wrapped self)
          nixops-built
        ];
      };
    };

    pkgs = import nixpkgs {
      overlays = [
        niv-overlay
        nixops-overlay
        (import ./overlays/nix-linter)
      ];
      config = {};
    };

    files = "$(find . -name '*.nix')";

    lint = pkgs.writeShellScriptBin "lint" ''
      ${pkgs.nix-linter}/bin/nix-linter ${files} "$@"
    '';

    format = pkgs.writeShellScriptBin "format" ''
      ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt ${files} "$@"
    '';

    deploy = pkgs.writeShellScriptBin "deploy" ''
      ${pkgs.nixops}/bin/nixops deploy "$@"
    '';

    collect-garbage = pkgs.writeShellScriptBin "collect-garbage" ''
      sudo ${pkgs.nix}/bin/nix-collect-garbage -d
    '';
  in

    pkgs.mkShell {
      buildInputs = [
        pkgs.git
        pkgs.niv
        pkgs.nixops
        lint
        format
        deploy
        collect-garbage
      ];
    }
