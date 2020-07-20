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

    nixops-overlay = _: _: {
      nixops = import nixops;
    };

    nixops-wrapped-overlay = self: _: {
      nixops-wrapped = self.symlinkJoin {
        name = "nixops";
        paths = [
          (self.callPackage ./nixops-deploy-wrapper.nix { inherit nixpkgs; })
          self.nixops
        ];
      };
    };

    pkgs = import nixpkgs {
      overlays = [
        niv-overlay
        nixops-overlay
        nixops-wrapped-overlay
        (import ./overlays/get-aws-access-key)
        (import ./overlays/nix-linter)
      ];
      config = {};
    };

    files = "$(find . -name '*.nix')";

    lint = pkgs.writeShellScriptBin "lint" ''
      ${pkgs.nix-linter}/bin/nix-linter ${files}
    '';

    format = pkgs.writeShellScriptBin "format" ''
      ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt ${files}
    '';

    deploy = pkgs.writeShellScriptBin "deploy" ''
      ${pkgs.nixops-wrapped}/bin/nixops deploy "$@"
    '';

    collect-garbage = pkgs.writeShellScriptBin "collect-garbage" ''
      sudo ${pkgs.nix}/bin/nix-collect-garbage -d
    '';
  in

    pkgs.mkShell {
      buildInputs = [
        pkgs.git
        pkgs.niv
        pkgs.nixops-wrapped
        lint
        format
        deploy
        collect-garbage
      ];
    }
