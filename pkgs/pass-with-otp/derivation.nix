{
  lib,
  pass,
}: let
  # In nixpkgs pass uses itself as a dependency.  Here we override that
  # dependency so it doesn't try to consume the pass package from the overlay
  # we're building, which would result in an infinite loop.
  pass' = pass.override {inherit pass;};
  withExtensions = passedExtensions:
    pass'.withExtensions (exts: lib.unique (passedExtensions exts ++ [exts.pass-otp]));
in
  (withExtensions (_: [])).overrideAttrs (_: {
    passthru = {
      inherit withExtensions;
      inherit (pass') extensions;
    };
  })
