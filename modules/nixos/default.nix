{ ... }:

let
  sources = import ../../sources.nix;
in

{
  imports = [
    (import ../../pkgs/mautrix-syncproxy/nixos-module.nix { src = sources.mautrix-syncproxy; })
    (import ../../pkgs/mautrix-wsproxy/nixos-module.nix { src = sources.mautrix-wsproxy; })
    ./autologin-graphical-session.nix
    ./color-theme.nix
    ./detachedLuksWithNixopsKeys.nix
    ./keymap.nix
    ./mautrix-signal.nix
    ./network-interfaces.nix
    ./powerpanel.nix
    ./preLVMTempMount.nix
    ./primary-user.nix
    ./route53DynamicDns.nix
    ./secure.nix
    ./sudo-cmds.nix
    ./umask.nix
  ];
}
