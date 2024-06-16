{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
    ...
  } @ flake-inputs:
    (
      flake-utils.lib.eachDefaultSystem
      (
        system: let
          password-utils-overlay = final: _: {
            inherit (final.callPackage ./lib/passwords.nix {}) passwordUtils;
          };

          cli-overlay = final: _: {
            cli = final.callPackage ./cli.nix {};
          };

          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              password-utils-overlay
              mkCli.overlays.default
              cli-overlay
            ];
            config = {};
          };
        in {
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
      colmena = let
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
      in {
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

        crux = mkMachine {
          targetHost = "crux";
        };
      };

      packages = let
        release = import "${nixpkgs}/nixos/release.nix";

        isoDir = ./isos;

        mkMinimalIso = name:
          builtins.mapAttrs (_: drv: {"${name}" = drv;})
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

        isos = mkMinimalIso "gpg-offline";
      in
        isos;
    };
}
