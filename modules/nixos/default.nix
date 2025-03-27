{...}: {
  imports = [
    ./backup.nix
    ./color-theme.nix
    ./detachedLuksWithNixopsKeys.nix
    ./keymap.nix
    ./network-interfaces.nix
    ./powerpanel.nix
    ./preLVMTempMount.nix
    ./primary-user.nix
    ./route53DynamicDns.nix
    ./sudo-cmds.nix
    ./umask.nix
  ];
}
