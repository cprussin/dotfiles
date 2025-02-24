{
  lib,
  pkgs,
  ...
}: let
  sources = import ../sources.nix;

  solarized-dark = pkgs.callPackage ../lib/color-themes/solarized/dark.nix {};

  update-key-expiry = pkgs.writeShellScriptBin "update-key-expiry" ''
    set -e

    echo
    echo -ne "\e[1;37mSearching for master...\e[0m "
    while ! $(sudo zpool import master 2>/dev/null); do
        echo -n "."
        sleep 0.5
    done
    echo
    sudo zfs load-key master/enc
    sudo zfs mount -o ro master/enc

    echo
    echo -e "\e[1;37mMounting liveusb...\e[0m"
    sudo mkdir /liveusb
    sudo mount /dev/disk/by-label/EFIBOOT /liveusb

    echo
    echo -e "\e[1;37mImporting secret key...\e[0m"
    gpg --pinentry-mode loopback --passphrase-fd 3 --import /master/enc/seckey.asc 3</master/enc/pw

    echo
    echo -e "\e[1;37mUpdating expiry on subkeys for connor@prussin.net...\e[0m"
    printf "key 1\nkey 2\nkey 3\nexpire\ny\n1m\nsave\n" | gpg --batch --command-fd 0 --status-fd=2 --pinentry-mode loopback --passphrase-fd 3 --edit-key connor@prussin.net 3</master/enc/pw

    echo
    echo -e "\e[1;37mExporting connor@prussin.net public keys to /connor@prussin.net-pubkey.asc on liveusb...\e[0m"
    gpg --batch --export --armor connor@prussin.net | sudo tee /liveusb/connor@prussin.net-pubkey.asc

    echo
    echo -e "\e[1;37mEjecting liveusb...\e[0m"
    sudo umount /liveusb

    echo
    echo -e "\e[1;37mEjecting master...\e[0m"
    sudo zpool export master

    echo
    echo
    echo -e "\e[1;37mAll done, please reboot and extract public key from /pubkey.asc"
    echo "on the live usb.  Make sure to deploy the new public key to website,"
    echo "dotfiles, keyservers (keyserver.ubuntu.com, keys.openpgp.org, pgp.mit.edu),"
    echo -e "phone, and github.\e[0m"
  '';
in {
  nixpkgs = {
    config.allowBroken = true;
    overlays = [
      (
        import ../pkgs/dircolors-solarized/overlay.nix {
          src = sources.dircolors-solarized;
        }
      )
    ];
  };

  isoImage = {
    isoBaseName = lib.mkForce "gpg-offline";
    appendToMenuLabel = " GPG Offline System";
  };

  boot = {
    kernelParams = ["copytoram"];
    tmp.cleanOnBoot = true;
    kernel.sysctl."kernel.unprivileged_bpf_disabled" = 1;
    initrd.network.enable = false;
  };

  services = {
    pcscd.enable = true;
    gpm.enable = true;
    openssh.enable = lib.mkForce false;
    udev.packages = [pkgs.yubikey-personalization];
    getty = {
      greetingLine = lib.mkForce "> GPG Offline System <";
      helpLine = lib.mkForce "Useful commands:\n  - `update-key-expiry`";
    };
  };

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-curses;
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

  console = {
    keyMap = pkgs.runCommand "console-keymap" {} ''
      ${pkgs.ckbcomp}/bin/ckbcomp \
        -layout us \
        -option caps:escape \
        -variant dvp > "$out"
    '';
    colors = map (builtins.replaceStrings ["#"] [""]) [
      solarized-dark.background
      solarized-dark.red
      solarized-dark.green
      solarized-dark.yellow
      solarized-dark.blue
      solarized-dark.purple
      solarized-dark.cyan
      solarized-dark.foreground
      solarized-dark.grey
      solarized-dark.lightRed
      solarized-dark.lightGreen
      solarized-dark.lightYellow
      solarized-dark.lightBlue
      solarized-dark.lightPurple
      solarized-dark.lightCyan
      solarized-dark.white
    ];
  };

  environment = {
    systemPackages = [
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
      update-key-expiry
    ];

    etc."inputrc".text = "set editing-mode vi";

    interactiveShellInit = "unset HISTFILE";
  };
}
