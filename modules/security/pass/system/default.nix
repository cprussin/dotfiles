{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      pass = self.writeShellScriptBin "pass" ''
        exec ${self.sudo}/bin/sudo -u ${config.primaryUserName} \
          ${self.callPackage ../pass.nix { pass = super.pass; secure = config.secure; }}/bin/pass "$@"
      '';
    })
  ];
}
