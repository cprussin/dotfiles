{ callPackage, emacs }:

{
  browse = callPackage ./browse.nix {};
  launch = callPackage ./launch.nix {};
  open = callPackage ./open.nix { inherit emacs; };
  prompt = callPackage ./prompt.nix {};
  search = callPackage ./search.nix {};
  yes-no = callPackage ./yes-no.nix {};
}
