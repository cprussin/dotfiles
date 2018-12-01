{ callPackage }:

{
  libuv_1_24_0 = callPackage ./libraries/libuv_1_24_0 {};
  metatron-cli = callPackage ./netflix/metatron-cli {};
  metatron-python = callPackage ./netflix/metatron-python {};
  nodejs_10_14_1 = callPackage ./javascript/nodejs_10_14_1 {};
  shakti = callPackage ./netflix/shakti {};
  yarn_1_12_3 = callPackage ./javascript/yarn_1_12_3 {};
}
