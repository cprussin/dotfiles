{...}: {
  imports = [
    ./autologin-graphical-session.nix
    ./backup.nix
    ./color-theme.nix
    ./detachedLuksWithNixopsKeys.nix
    ./keymap.nix
    ./mautrix-signal.nix
    ./network-interfaces.nix
    ./powerpanel.nix
    ./preLVMTempMount.nix
    ./primary-user.nix
    ./route53DynamicDns.nix
    ./sudo-cmds.nix
    ./umask.nix
  ];
}
