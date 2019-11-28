{ pkgs, ... }:

{
  nix.extraOptions = ''
    plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins
    extra-builtins-file = ${toString ./extra-builtins.nix}
  '';
}
