{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    colmena.url = "github:zhaofengli/colmena";

    dircolors-solarized = {
      url = "github:seebi/dircolors-solarized";
      flake = false;
    };
    fzf-pass = {
      url = "github:cprussin/fzf-pass";
      flake = false;
    };
    gpg-key = {
      url = "https://connor.prussin.net/pubkey.asc";
      flake = false;
    };
    mkCli.url = "github:cprussin/mkCli";
    gmail-new-mail-counter.url = "github:cprussin/gmail-new-mail-counter";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    notify-send = {
      url = "github:vlevit/notify-send.sh";
      flake = false;
    };
    zoom-frm = {
      url = "github:cprussin/zoom-frm";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    flake-utils,
    mkCli,
    home-manager,
    nixos-hardware,
    colmena,
    ...
  } @ flake-inputs: let
    password-utils-overlay = final: _: {
      inherit (final.callPackage ./lib/passwords.nix {}) passwordUtils;
    };

    cli-overlay = final: _: {
      cli = final.callPackage ./cli.nix {};
    };

    iot-overlay = final: _: {
      iot-devices = final.callPackage ./config/iot {};
    };

    release = import "${nixpkgs}/nixos/release.nix";

    isoDir = ./isos;

    mkMinimalIso = name:
      (release {
        nixpkgs = {
          inherit (nixpkgs) outPath;
          revCount = 0;
          shortRev = builtins.substring 0 7 nixpkgs.rev;
        };
        stableBranch = true;
        supportedSystems = ["x86_64-linux"];
        configuration = "${toString isoDir}/${name}.nix";
      })
      .iso_minimal;

    mkMinimalIsos = isos: builtins.listToAttrs (map (name: { inherit name; value = mkMinimalIso name; }) isos);

    isos = mkMinimalIsos [
      "gpg-offline"
      "installer"
    ];

    # See https://jade.fyi/blog/flakes-arent-real/ for why we do this and
    # don't use `specialArgs`.
    injectFlakeInputs = {lib, ...}: {
      options.flake-inputs = lib.mkOption {
        type = lib.types.attrsOf lib.types.unspecified;
      };
      config = {
        inherit flake-inputs;
      };
    };

    machineDir = ./config/machines;

    mkMachine = {
      targetHost,
      extraModules ? [],
    }: {
      deployment = {inherit targetHost;};
      imports =
        extraModules
        ++ [
          home-manager.nixosModules.home-manager
          injectFlakeInputs
          "${toString machineDir}/${targetHost}"
        ];
    };

    colmena-machines = {
      meta = {
        nixpkgs = import nixpkgs {
          system = "x86_64-linux";
        };
      };

      aries = mkMachine {
        targetHost = "aries";
        extraModules = [
          nixos-hardware.nixosModules.framework-11th-gen-intel
        ];
      };

      lyra = mkMachine {
        targetHost = "lyra";
      };

      crux = mkMachine {
        targetHost = "crux";
      };
    };
  in
    (
      flake-utils.lib.eachDefaultSystem
      (
        system: let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              colmena.overlays.default
              password-utils-overlay
              mkCli.overlays.default
              cli-overlay
              iot-overlay
            ];
            config = {};
          };
        in {
          packages =
            (builtins.mapAttrs (_: value: value."${system}") isos)
            // {
              inherit (pkgs) iot-devices;
            };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.git
              pkgs.passwordUtils
              pkgs.colmena
              pkgs.cli
            ];
          };
        }
      )
    )
    // {
      colmena = colmena-machines;
      colmenaHive = colmena.lib.makeHive colmena-machines;
      overlays.iot-devices = iot-overlay;
    };
}
