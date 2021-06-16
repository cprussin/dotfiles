let
  sources = import ./sources.nix;
in
{ nixpkgs ? sources.nixpkgs
, niv ? sources.niv
, nixops ? sources.nixops
, nixpkgs-unstable ? sources.nixpkgs-unstable
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

  mkExtraBuiltinsCli = pkgs: opts: builtins.concatStringsSep " " (
    pkgs.lib.mapAttrsToList (option: value: "--option ${option} ${value}") opts
  );

  extraBuiltinsOptions = pkgs: mkExtraBuiltinsCli pkgs {
    plugin-files = "${pkgs.nix-plugins}/lib/nix/plugins/libnix-extra-builtins.so";
    extra-builtins-file = "$(nix-build --no-out-link)/extra-builtins.nix";
  };

  nixops-wrapped = pkgs: pkgs.writeShellScriptBin "nixops" ''
    cmd=$1
    shift

    export NIX_PATH="nixpkgs=${nixpkgs}"

    exec ${nixops-built}/bin/nixops $cmd ${extraBuiltinsOptions pkgs} "$@"
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

  password-utils-overlay = self: _: {
    passwordUtils = (self.callPackage ./lib/passwords.nix { }).passwordUtils;
  };

  pkgs-unstable = import nixpkgs-unstable { };

  esphome-overlay = _: _: { inherit (pkgs-unstable) esphome; };

  pkgs = import nixpkgs {
    overlays = [
      niv-overlay
      nixops-overlay
      password-utils-overlay
      esphome-overlay
    ];
    config = { };
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

  iot = pkgs.writeShellScriptBin "iot" ''
    set -e

    rm -f iot-build/result.yaml*
    nix-build ${extraBuiltinsOptions pkgs} --out-link "iot-build/result.yaml" --attr "$2" ./config/iot
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
}
