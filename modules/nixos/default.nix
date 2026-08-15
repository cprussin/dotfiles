{...}: {
  imports = [
    ./backup.nix
    ./color-theme.nix
    ./detachedLuksWithNixopsKeys.nix
    ./keymap.nix
    ./luksWithKeyDrive.nix
    ./network-interfaces.nix
    ./powerpanel.nix
    ./primary-user.nix
    ./route53DynamicDns.nix
    ./sudo-cmds.nix
    ./umask.nix
  ];
}
