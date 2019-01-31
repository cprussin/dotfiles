{ ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      pass = super.pass.withExtensions (exts: [ exts.pass-otp ]);
    })
  ];
}
