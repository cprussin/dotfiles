{
  config,
  lib,
  pkgs,
  ...
}: {
  # `pkgs.claude-desktop`, which ui/domicile's launcher entry runs.
  nixpkgs.overlays = [
    (import ../../../../pkgs/claude-desktop/overlay.nix {
      aptIndexes = {
        x86_64-linux = config.flake-inputs.claude-desktop-apt-amd64;
        aarch64-linux = config.flake-inputs.claude-desktop-apt-arm64;
      };
    })
  ];

  # The app runs its Code sessions -- Remote Control included, which is how
  # claude.ai starts sessions on this machine -- with a Claude Code CLI it
  # downloads itself into ~/.config/Claude/claude-code.  That is a generic
  # glibc binary asking for /lib64/ld-linux-x86-64.so.2, which NixOS does not
  # have, so without this every session dies at exec and the only way in is
  # running `claude remote-control` by hand.  It links glibc only, so the
  # default library set is enough.
  programs.nix-ld.enable = true;

  # Cowork runs its sandbox VM through KVM, and it opens /dev/vhost-vsock,
  # which only members of this group can, whatever /dev/kvm's own permissions
  # are.  See https://code.claude.com/docs/en/desktop-linux
  primary-user.extraGroups = ["kvm"];

  # Its desktop entry is what handles claude:// -- the sign-in link the browser
  # opens -- so it has to be in the profile, not only behind ui/domicile's
  # launcher entry.  The launcher omits it.  `lib.mkForce` as ui/xdg-portal
  # explains.
  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.claude-desktop];
    xdg.mimeApps.defaultApplications."x-scheme-handler/claude" = "com.anthropic.Claude.desktop";
  };

  # The Secret Service the app keeps its sign-in in; without one it shows a
  # "sign in again" banner after every login.  The first sign-in prompts for a
  # new keyring's password; leave it blank: autologin has nothing to unlock it
  # with, and the disk is already encrypted.
  services.gnome.gnome-keyring.enable = true;
}
