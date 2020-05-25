let
  sources = import ../../sources.nix;
in

{ nixpkgs ? sources.nixpkgs
, zsh-git-prompt ? sources.zsh-git-prompt
}:

  let
    pkgs = import nixpkgs {
      overlays = [
        (import ./overlay.nix { src = zsh-git-prompt; })
      ];
    };
  in

    pkgs.zsh-git-prompt
