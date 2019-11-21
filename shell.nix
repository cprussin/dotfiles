{ nixpkgs ? <nixpkgs>
, nix-linter ? fetchTarball "https://github.com/Synthetica9/nix-linter/tarball/master"
}:

let
  pkgs = import nixpkgs {};
  nix-linter-pkgs = pkgs.callPackage nix-linter {};
in

pkgs.mkShell {
  buildInputs = [
    pkgs.git
    pkgs.nixpkgs-fmt
    nix-linter-pkgs.nix-linter
  ];
}
