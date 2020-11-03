{ symlinkJoin, callPackage }:

symlinkJoin {
  name = "get-aws-access-key";
  paths = [
    (callPackage ./scripts/get-aws-access-key.nix { })
    (callPackage ./scripts/get-aws-access-key-nixops.nix { })
  ];
}
