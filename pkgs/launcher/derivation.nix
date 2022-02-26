{
  symlinkJoin,
  callPackage,
}:
symlinkJoin {
  name = "launcher";
  paths = [
    (callPackage ./scripts/browse.nix {})
    (callPackage ./scripts/launch.nix {})
    (callPackage ./scripts/open.nix {})
    (callPackage ./scripts/preview.nix {})
    (callPackage ./scripts/run.nix {})
    (callPackage ./scripts/search.nix {})
  ];
}
