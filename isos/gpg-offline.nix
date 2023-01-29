{
  lib,
  pkgs,
  ...
}: let
  gpg-agent-conf = pkgs:
    pkgs.writeText "gpg-agent.conf" ''
      pinentry-program ${pkgs.pinentry.curses}/bin/pinentry
    '';

  gpg-conf = pkgs: pkgs.writeText "gpg.conf" "no-symkey-cache";
in {
  nixpkgs.config.allowBroken = true;
  isoImage.isoBaseName = lib.mkForce "nixos-yubikey";

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = ["copytoram"];
    cleanTmpDir = true;
    kernel.sysctl."kernel.unprivileged_bpf_disabled" = 1;
    initrd.network.enable = false;
  };

  services = {
    pcscd.enable = true;
    gpm.enable = true;
    udev.packages = [pkgs.yubikey-personalization];
  };

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  networking = {
    dhcpcd = {
      enable = false;
      allowInterfaces = [];
    };
    interfaces = {};
    firewall.enable = true;
    useDHCP = false;
    useNetworkd = false;
    wireless.enable = false;
    networkmanager.enable = lib.mkForce false;
  };

  console.keyMap = pkgs.runCommand "console-keymap" {} ''
    ${pkgs.ckbcomp}/bin/ckbcomp \
      -layout us \
      -option caps:escape \
      -variant dvp > "$out"
  '';

  environment = {
    systemPackages = [
      (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hopenpgp-tools)
      pkgs.cfssl
      pkgs.cryptsetup
      pkgs.diceware
      pkgs.ent
      pkgs.gptfdisk
      pkgs.paperkey
      pkgs.pcsctools
      pkgs.pgpdump
      pkgs.pwgen
      pkgs.yubico-piv-tool
      pkgs.yubikey-manager
      pkgs.yubikey-personalization
    ];

    etc."inputrc".text = "set editing-mode vi";

    interactiveShellInit = ''
      unset HISTFILE

      export GNUPGHOME="/run/user/$(id -u)/gnupg"
      if [ ! -d "$GNUPGHOME" ]; then
        echo "Creating \$GNUPGHOME…"
        install --verbose -m=0700 --directory="$GNUPGHOME"
      fi
      [ ! -f "$GNUPGHOME/gpg.conf" ] && cp --verbose ${gpg-conf pkgs} "$GNUPGHOME/gpg.conf"
      [ ! -f "$GNUPGHOME/gpg-agent.conf" ] && cp --verbose ${gpg-agent-conf pkgs} "$GNUPGHOME/gpg-agent.conf"
      echo "\$GNUPGHOME is \"$GNUPGHOME\""
    '';
  };
}
