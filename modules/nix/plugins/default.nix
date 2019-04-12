{ pkgs, ... }:

{
  nix.extraOptions = ''
    plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins
    extra-builtins-file = ${./extra-builtins.nix}
  '';
}
