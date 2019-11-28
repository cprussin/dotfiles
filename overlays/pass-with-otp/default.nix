self: super:
let
  pkg = passedExtensions: super.pass.withExtensions (
    exts: self.lib.unique (passedExtensions exts ++ [ exts.pass-otp ])
  );
in

{
  pass = pkg (_: []) // { withExtensions = pkg; };
}
