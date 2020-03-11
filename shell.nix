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
    (pkgs.lib.mapAttrsToList export-password-data exported-password-data) ++ [
      (pack-pass "public-ssh-key" "$(gpg --export-ssh-key $(cat $PASSWORD_STORE_DIR/.gpg-id))")
    ]
  );

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
      sudo PASS_DATA="${pass-data}" ${deploy-root-cmd} $1
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
    pkgs.nix-linter
    niv
    lint
    format
    deploy
    collect-garbage
  ];
}
