{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      pass = self.writeShellScriptBin "pass" ''
        export PASSWORD_STORE_DIR=${config.secure.passwords}
        exec ${super.pass.withExtensions (exts: [ exts.pass-otp ])}/bin/pass "$@"
      '';
    })
  ];
}
