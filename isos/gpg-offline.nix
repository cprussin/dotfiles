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
    echo -e "\e[1;37mImporting master...\e[0m"
    sudo zpool import master
    sudo zfs load-key master/enc
    sudo zfs mount master/enc

    echo
    echo -e "\e[1;37mMounting liveusb...\e[0m"
    sudo mkdir /liveusb
    sudo mount /dev/disk/by-label/EFIBOOT /liveusb

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
    echo -e "\e[1;37mSnapshotting master...\e[0m"
    oldsnap="$(zfs list -t snapshot -o name -s creation -r master/enc | tail -1 | sed 's/^master\/enc@//')"
    newsnap="$(date +%F)"
    sudo zfs snapshot master/enc@$newsnap

    echo
    echo -e "\e[1;37mScrubbing master...\e[0m"
    sudo zpool scrub -w master
    sudo zpool status master

    echo
    echo -e "\e[1;37mRunning backups...\e[0m"
    while true; do
      echo
      echo
      echo -ne "\e[1;37mAre there more filesystems to back up (Y/n)?\e[0m "
      read button
      if [ "$button" == "n" ]; then
        echo
        echo -e "\e[1;37mEjecting master filesystem...\e[0m"
        sudo zpool export master
        echo
        echo
        echo -e "\e[1;37mAll done, please reboot and extract public key from /pubkey.asc"
        echo "on the live usb.  Make sure to deploy the new public key to"
        echo "dotfiles, keyservers (keyserver.ubuntu.com, keys.openpgp.org,"
        echo -e "pgp.mit.edu), phone, website, and github.\e[0m"
        exit
      else
        echo -ne "\e[1;37mPlease insert next filesystem\e[0m "
        while ! $(sudo zpool import -t master master-bak 2>/dev/null); do
            echo -n "."
            sleep 0.5
        done
        echo
        sudo zfs load-key master-bak/enc
        sudo zfs mount master-bak/enc
        echo
        sudo zfs send -I master/enc@$oldsnap master/enc@$newsnap | sudo zfs recv -Fd master-bak/enc
        sudo zpool scrub -w master-bak
        zpool status master-bak
        sudo zpool export master-bak
        echo -ne "\e[1;37mPlease remove filesystem\e[0m "
        while [ "$(sudo zpool import 2>/dev/null | grep master)" ]; do
            echo -n "."
            sleep 0.5
        done
      fi
    done
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

    interactiveShellInit = ''
      unset HISTFILE
      export GNUPGHOME=/master/enc/gnupg
    '';
  };
}
