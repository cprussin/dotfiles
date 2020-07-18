{ nixpkgs, lib, writeShellScriptBin, nixops, get-aws-access-key, nix-plugins }:

let
  build-nix-path-env-var = path:
    builtins.concatStringsSep ":" (
      lib.mapAttrsToList (k: v: "${k}=${v}") path
    );

  nix-path = build-nix-path-env-var {
    inherit nixpkgs;
    nixpkgs-overlays = "$(NIX_PATH=nixpkgs=${nixpkgs} nix-build --no-out-link)/overlays";
    nixos-config = "$(NIX_PATH=nixpkgs=${nixpkgs} nix-build --no-out-link)/config/machines/$(hostname)";
  };
in

writeShellScriptBin "nixops" ''
  set -e

  if [ $1 == "deploy" ]
  then
    if ! $(mount | grep /secure >/dev/null)
    then
      echo "/secure is not mounted!"
      exit 1
    fi

    shift

    NIX_PATH="${nix-path}" \
      ${get-aws-access-key}/bin/get-aws-access-key-nixops deploy \
      --option plugin-files ${nix-plugins}/lib/nix/plugins/libnix-extra-builtins.so \
      --option extra-builtins-file ${./extra-builtins.nix} \
      "$@"
  else
    ${nixops}/bin/nixops "$@"
  fi
''
