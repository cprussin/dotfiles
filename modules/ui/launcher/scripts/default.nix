{ callPackage, emacs, config }:

{
  browse = callPackage ./browse.nix {};
  launch = callPackage ./launch.nix {};
  open = callPackage ./open.nix { inherit emacs; };
  prompt = callPackage ./prompt.nix { inherit config; };
  search = callPackage ./search.nix {};
  yes-no = callPackage ./yes-no.nix {};
}
