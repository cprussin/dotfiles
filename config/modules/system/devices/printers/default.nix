{ pkgs, ... }:

{
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplip ];
  };
  # TODO add declarative printer config when 19.09 lands
  # see https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/hardware/printers.nix
}
