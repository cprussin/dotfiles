{...}: {
  imports = [
    ./backup.nix
    ./batched-deployment-keys.nix
    ./color-theme.nix
    ./detachedLuksWithNixopsKeys.nix
    ./keymap.nix
    ./luksWithKeyDrive.nix
    ./network-interfaces.nix
    ./powerpanel.nix
    ./primary-user.nix
    ./sudo-cmds.nix
    ./umask.nix
  ];
}
