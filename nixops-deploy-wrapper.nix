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

  cmd=$1
  if [ $cmd == "deploy" ]
  then
    bin=${get-aws-access-key}/bin/get-aws-access-key-nixops
  else
    bin=${nixops}/bin/nixops
  fi

  shift

  NIX_PATH="${nix-path}" $bin $cmd \
    --option plugin-files ${nix-plugins}/lib/nix/plugins/libnix-extra-builtins.so \
    --option extra-builtins-file ${./extra-builtins.nix} \
    "$@"
''
