{ sources ? import ./sources.nix }:

let
  pkgs = import sources.nixpkgs {};

  niv = pkgs.symlinkJoin {
    name = "niv";
    paths = [ sources.niv ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/niv \
        --add-flags "--sources-file ${toString ./sources.json}"
    '';
  };

  nix-linter = pkgs.callPackage sources.nix-linter {};

  build-nix-path-env-var = path:
    builtins.concatStringsSep ":" (
      pkgs.lib.mapAttrsToList (k: v: "${k}=${v}") path
    );

  nix-path = build-nix-path-env-var {
    nixpkgs = sources.nixpkgs;
    nixpkgs-overlays = "$dotfiles/overlays";
    nixos-config = "$dotfiles/current-machine";
    home-manager = sources.home-manager;
  };

  files = "$(find . -name '*.nix' -not -wholename './sources.nix')";

  lint = pkgs.writeShellScriptBin "lint" "nix-linter ${files}";

  format = pkgs.writeShellScriptBin "format" "nixpkgs-fmt ${files}";

  deploy-root-cmd = pkgs.writeShellScript "deploy-root-cmd" ''
    export dotfiles="$(nix-build --no-out-link)"
    export NIX_PATH="${nix-path}"
    nixos-rebuild switch --show-trace
  '';

  deploy = pkgs.writeShellScriptBin "deploy" ''
    set -e
    lint
    format

    if ! $(mount | grep /boot >/dev/null)
    then
      echo "/boot is not mounted!"
      exit 1
    fi

    if ! $(mount | grep /secure >/dev/null)
    then
      echo "/secure is not mounted!"
      exit 1
    fi

    if [ ! -s ./current-machine ]
    then
      echo "You must link to a current-machine"
      echo "Try \`ln -s ./machines/<machine-name> current-machine\`"
      exit 1
    fi

    sudo sh -c ${deploy-root-cmd}
  '';

  collect-garbage =
    pkgs.writeShellScriptBin "collect-garbage" "sudo nix-collect-garbage -d";
in

pkgs.mkShell {
  buildInputs = [
    pkgs.git
    pkgs.nixpkgs-fmt
    niv
    nix-linter.nix-linter
    lint
    format
    deploy
    collect-garbage
  ];
}
