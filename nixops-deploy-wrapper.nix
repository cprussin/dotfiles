{ nixpkgs, lib, writeShellScriptBin, nixops, get-aws-access-key }:

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

  exported-password-data = {
    "Netflix/Domain" = [ "Username" "Password" ];
    "Wifi/Centar" = [ "Password" ];
    "Wifi/CentarPhone" = [ "Password" ];
    "Wifi/CentarCar" = [ "Password" ];
    "Wifi/PC House2" = [ "Password" ];
    "Netflix/VPN/ca" = [ "Full" ];
    "Netflix/VPN/tls-auth" = [ "Full" ];
    "Netflix/VPN/cert" = [ "Full" ];
    "Netflix/VPN/key" = [ "Full" ];
  };

  pack-pass = name: value: "\\\"${name}\\\":\\\"${value}\\\"";

  export-password-field = pass: field:
    pack-pass field (
      if field == "Full"
      then "$(pass show '${pass}' | awk -v ORS='\\\\n' '1')"
      else
        if field == "Password"
        then "$(pass show '${pass}' | head -n 1)"
        else "$(pass show '${pass}' | grep '^${field}: ' | sed 's/^${field}: //')"
    );

  mk-json-string = data: "{" + (builtins.concatStringsSep "," data) + "}";

  export-password-data = pass: fields:
    "\\\"${pass}\\\":${mk-json-string (map (export-password-field pass) fields)}";

  pass-data = mk-json-string (
    (lib.mapAttrsToList export-password-data exported-password-data) ++ [
      (pack-pass "public-ssh-key" "$(gpg --export-ssh-key $(cat $PASSWORD_STORE_DIR/.gpg-id))")
    ]
  );
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

    NIX_PATH="${nix-path}" PASS_DATA="${pass-data}" \
      ${get-aws-access-key}/bin/get-aws-access-key-nixops "$@"
  else
    ${nixops}/bin/nixops "$@"
  fi
''
