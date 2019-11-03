{ ... }:

let
  nixosVersion = import ../../../nixos-version.nix;
  home-manager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/release-${nixosVersion}.tar.gz";
in

{
  imports = [ "${home-manager}/nixos" ];
}
