{ callPackage }:

{
  metatron-cli = callPackage ./netflix/metatron-cli {};
  metatron-python = callPackage ./netflix/metatron-python {};
  shakti = callPackage ./netflix/shakti {};
  yarn = callPackage ./javascript/yarn {};
}
