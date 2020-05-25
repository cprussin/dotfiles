{ lib, pass }:

let
  pkg = passedExtensions: pass.withExtensions (
    exts: lib.unique (passedExtensions exts ++ [ exts.pass-otp ])
  );
in

pkg (_: []) // { withExtensions = pkg; }
