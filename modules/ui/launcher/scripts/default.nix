{ callPackage, config }:

{
  browse = callPackage ./browse.nix {};
  launch = callPackage ./launch.nix {};
  open = callPackage ./open.nix {};
  prompt = callPackage ./prompt.nix { inherit config; };
  search = callPackage ./search.nix {};
  yes-no = callPackage ./yes-no.nix {};
}
