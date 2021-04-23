{ callPackage }:
let
  mkWebApp = callPackage ./mkWebApp.nix { };
in
name: url: mkWebApp name (
  builtins.replaceStrings [ "@user@" ] [ "connor@prussin.net" ] url
)
