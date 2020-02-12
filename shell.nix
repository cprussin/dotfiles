{ sources ? import ./sources.nix }:

let
  pkgs = import sources.nixpkgs {};

  build-nix-path-env-var = path:
    builtins.concatStringsSep ":" (
      pkgs.lib.mapAttrsToList (k: v: "${k}=${v}") path
    );

  nix-path = build-nix-path-env-var {
    nixpkgs = sources.nixpkgs;
    nixpkgs-overlays = "$dotfiles/overlays";
    nixos-config = "$dotfiles/current-machine";
  };

  files = "$(find . -name '*.nix')";

  lint = pkgs.writeShellScriptBin "lint" "nix-linter ${files}";

  format = pkgs.writeShellScriptBin "format" "nixpkgs-fmt ${files}";

  set-nix-path = ''
    export dotfiles="$(nix-build --no-out-link)"
    export NIX_PATH="${nix-path}"
  '';

  deploy-root-cmd = pkgs.writeShellScript "deploy-root-cmd" ''
    ${set-nix-path}
    nixos-rebuild ''${1-switch} --show-trace
  '';

  deploy = pkgs.writeShellScriptBin "deploy" ''
    set -e
    ${lint}/bin/lint
    ${format}/bin/format

    if ! $(mount | grep /secure >/dev/null)
    then
      echo "/secure is not mounted!"
      exit 1
    fi

    if [ "$1" -a -d config/networks/$1 ]
    then
      echo Deploying network $1...
      ${set-nix-path}
      get-aws-access-key-nixops deploy -d $1 --show-trace
    else
      if ! $(mount | grep /boot >/dev/null)
      then
        echo "/boot is not mounted!"
        exit 1
      fi

      if [ ! -s ./current-machine ]
      then
        echo "You must link to a current-machine"
        echo "Try \`ln -s ./machines/<machine-name> current-machine\`"
        exit 1
      fi

      echo Deploying local...
      sudo ${deploy-root-cmd} $1
    fi
  '';

  collect-garbage =
    pkgs.writeShellScriptBin "collect-garbage" "sudo nix-collect-garbage -d";
in

pkgs.mkShell {
  buildInputs = [
    pkgs.git
    pkgs.nixpkgs-fmt
    pkgs.get-aws-access-key
    pkgs.nixops
    pkgs.niv
    pkgs.nix-linter
    lint
    format
    deploy
    collect-garbage
  ];
}
