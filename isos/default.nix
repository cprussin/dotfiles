{
  sources ? import ../sources.nix,
  nixpkgs ? sources.nixpkgs,
}: let
  release = import "${nixpkgs}/nixos/release.nix";

  mkIso = configPath:
    release {
      nixpkgs = {
        inherit (nixpkgs) outPath;
        revCount = 0;
        shortRev = builtins.substring 0 7 nixpkgs.rev;
      };
      stableBranch = true;
      supportedSystems = ["x86_64-linux"];
      configuration = import configPath;
    };
in {
  gpg-offline = (mkIso ./gpg-offline.nix).iso_minimal;
}
