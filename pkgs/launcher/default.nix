{ symlinkJoin, callPackage }:

symlinkJoin {
  name = "launcher";
  paths = [
    (callPackage ./scripts/browse.nix {})
    (callPackage ./scripts/launch.nix {})
    (callPackage ./scripts/open.nix {})
    (callPackage ./scripts/search.nix {})
    (callPackage ./scripts/yes-no.nix {})
  ];
}
