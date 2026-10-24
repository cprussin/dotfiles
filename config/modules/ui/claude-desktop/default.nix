{config, ...}: {
  # `pkgs.claude-desktop`, which ui/domicile's launcher entry runs.
  nixpkgs.overlays = [
    (import ../../../../pkgs/claude-desktop/overlay.nix {
      aptIndexes = {
        x86_64-linux = config.flake-inputs.claude-desktop-apt-amd64;
        aarch64-linux = config.flake-inputs.claude-desktop-apt-arm64;
      };
    })
  ];

  # Cowork runs its sandbox VM through KVM, and it opens /dev/vhost-vsock,
  # which only members of this group can, whatever /dev/kvm's own permissions
  # are.  See https://code.claude.com/docs/en/desktop-linux
  primary-user.extraGroups = ["kvm"];
}
